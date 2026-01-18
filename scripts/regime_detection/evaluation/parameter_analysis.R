#
# Parameter Importance Analysis for Regime Detection
#
# Analyzes FLOSS hyperparameters (window_size, regime_threshold,
# regime_landmark, min_gap_samples) using BART model with FIRM,
# Permutation, and SHAP importance methods.
#
# Reads CSV output from Python evaluation (ts-segmentation project)
# with aggregated metrics per model configuration.
#

# =============================================================================
# CONFIGURATION
# =============================================================================
# nolint start
DATASET <- "afib_regimes" # Options: malignantventricular, afib_regimes, vtachyarrhythmias
METRIC <- "f3_weighted" # Options: f1_classic, f1_weighted, f3_classic, f3_weighted,
#          recall_4s, recall_10s, precision_4s, precision_10s,
#          edd_median_s, fp_per_min,
#          nab_score_standard, nab_score_low_fp, nab_score_low_fn

PARALLEL <- TRUE # Use parallel processing (20 cores available)

# When TRUE, make the custom pred.fun reproduce pdp's default behaviour for
# matrix predictions (get_predictions.default() takes pred[, 1] and then mean()).
# When FALSE, compute a more standard PDP: average predictions over *all*
# observations in `newdata` for each grid-point.
PDP_MATCH_PDP_DEFAULT <- FALSE

CV_FOLDS <- 5 # Cross-validation folds
CV_REPS <- 1 # CV repetitions
BART_GRID <- 30 # Grid size for BART tree tuning

# SHAP/Permutation iterations
NSIM_FIRM <- 100
NSIM_PERM <- 100
NSIM_SHAP <- 400

# Cache directory for fitted models
CACHE_DIR <- here::here("output", "parameter_analysis")
dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)
# nolint end
# =============================================================================
# SETUP
# =============================================================================

library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(cli)

# Check and load required packages
required_pkgs <- c(
  "dbarts", "vip", "pdp", "fastshap", "shapviz",
  "parsnip", "workflows", "tune", "rsample", "yardstick", "hardhat",
  "future", "parallelly"
)

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cli_alert_warning("Package {.pkg {pkg}} not installed. Installing...")
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# Load helper functions
source(here("scripts", "helpers", "parameter_analysis_helpers.R"))

cli_h1("FLOSS Parameter Importance Analysis")
cli_alert_info("Dataset: {.val {DATASET}}")
cli_alert_info("Metric: {.val {METRIC}}")
cli_alert_info("Parallel: {.val {PARALLEL}} ({parallelly::availableCores(methods = 'system')} cores available)")

# =============================================================================
# LOAD DATA
# =============================================================================

cli_h2("Loading Data")

csv_path <- here("output", "regime_detection", DATASET, "evaluation", "models_aggregated.csv")

if (!file.exists(csv_path)) {
  cli_abort("CSV file not found: {.path {csv_path}}")
}

cli_alert_info("Reading {.path {csv_path}}...")
raw_data <- readr::read_csv(csv_path, show_col_types = FALSE)

cli_alert_success("Loaded {nrow(raw_data)} model configurations")

# Define features and outcome
predictors_names <- c("window_size", "regime_threshold", "regime_landmark", "min_gap_samples")
outcome_name <- METRIC

# Check if metric exists
if (!outcome_name %in% colnames(raw_data)) {
  available_metrics <- setdiff(colnames(raw_data), c(
    "model_id", predictors_names, "n_records",
    "n_ground_truth", "n_detections", "tp", "fp", "fn", "tp_weight_sum"
  ))
  cli_abort("Metric {.val {outcome_name}} not found. Available: {.val {available_metrics}}")
}

# Prepare data for BART modeling
cli_alert_info("Preparing aggregated dataset for BART modeling...")
tree_data <- raw_data |>
  select(all_of(c(predictors_names, outcome_name))) |>
  rename(mean = all_of(outcome_name)) |>
  filter(!is.na(mean), is.finite(mean))

# Check for missing values
na_count <- sum(is.na(tree_data))
if (na_count > 0) {
  na_pct <- 100 * na_count / (nrow(tree_data) * ncol(tree_data))
  cli_alert_warning("{na_count} missing values ({round(na_pct, 2)}%) detected")

  if (na_pct > 10) {
    cli_abort("Too many NAs (>10%). Consider using a different metric.")
  }

  # Remove rows with NAs
  tree_data <- tree_data |> tidyr::drop_na()
  cli_alert_info("Removed NA rows. Remaining: {nrow(tree_data)} configurations")
}

cli_alert_success("Final dataset: {nrow(tree_data)} rows × {ncol(tree_data)} columns")

# use this range to compare with the interactions later
cli_alert_info("Metric range: [{round(min(tree_data$mean), 3)}, {round(max(tree_data$mean), 3)}]")

# =============================================================================
# TRAIN BART MODEL
# =============================================================================

cli_h2("Training BART Model")

cache_file <- file.path(CACHE_DIR, glue::glue("bart_fitted_{DATASET}_{METRIC}.rds"))

if (file.exists(cache_file)) {
  cli_alert_info("Loading cached model from {.path {cache_file}}...")
  trained_model <- readRDS(cache_file)
} else {
  cli_alert_info("Training BART model (CV: {CV_FOLDS}×{CV_REPS}, Grid: {BART_GRID})...")
  cli_alert_info("This may take 10-30 minutes depending on data size...")

  trained_model <- train_models(tree_data,
    parallel = PARALLEL,
    v = CV_FOLDS,
    rep = CV_REPS,
    grid = BART_GRID
  )

  cli_alert_info("Saving model to cache...")
  saveRDS(trained_model, file = cache_file)
  cli_alert_success("Model saved to {.path {cache_file}}")
}

train_data <- trained_model$training_data
testing_data <- trained_model$testing_data

cli_alert_success("Train/test split: {nrow(train_data)}/{nrow(testing_data)}")

cli_h3("Check final model performance on test data")
# Fit final model
cli_alert_info("Fitting best model on full training data...")
set.seed(102)
best_fit <- generics::fit(trained_model$model, train_data)
bart_engine <- workflows::extract_fit_engine(best_fit)

# Evaluate model performance
cli_alert_info("Evaluating model performance...")

pred <- predict(bart_engine, testing_data)
pred <- colMeans(pred)

rmse_val <- yardstick::rmse_vec(testing_data$mean, pred)
rsq_val <- yardstick::rsq_vec(testing_data$mean, pred)

cli_alert_success("Model engine RMSE: {.val {round(rmse_val, 4)}}")
cli_alert_success("Model engine R²: {.val {round(rsq_val, 4)}}")

if (rmse_val > 0.5 * sd(testing_data$mean)) {
  cli_abort("RMSE is high relative to data variance. Results may be unreliable.")
}

# =============================================================================
# INTERACTION ANALYSIS
# =============================================================================

cli_h2("Analyzing Parameter Interactions")

cache_interactions <- file.path(
  CACHE_DIR,
  glue::glue("interactions_{DATASET}_{METRIC}.rds")
)

if (file.exists(cache_interactions)) {
  cli_alert_info("Loading cached interactions...")
  interactions <- readRDS(cache_interactions)
} else {
  cli_alert_info("Computing 2-way interactions using PDP...")
  cli_alert_info("Expected pairs: {choose(length(predictors_names), 2)}")

  # IMPORTANT: the dbarts engine object is not reliably serializable.
  # saveRDS()/readRDS() (and PSOCK exports) can silently degrade predict()
  # to a constant output. For stable parallel PDP, initialize the engine
  # inside each worker process (external pointers are created per-process).
  model_cache_file <- cache_file

  all_pairs <- utils::combn(predictors_names, m = 2)
  all_pairs <- purrr::array_tree(all_pairs, 2)

  set.seed(123)
  partial_data <- train_data #|> dplyr::slice_sample(n = 100) # this is for debugging, use full data later

  pdp_start_time <- Sys.time()

  if (PARALLEL) {
    cli_alert_info("Parallel processing enabled for interaction computation")

    Sys.setenv("_R_CHECK_LIMIT_CORES_" = FALSE) # this is needed to use more than 2 cores

    system_cores <- parallelly::availableCores(methods = "system")
    cgroup_cores <- tryCatch(
      parallelly::availableCores(methods = "cgroups2.cpu.max"),
      error = function(e) NA_integer_
    )
    effective_cores <- if (is.na(cgroup_cores)) system_cores else min(system_cores, cgroup_cores)
    n_jobs <- max(1, effective_cores - 2)

    cli_alert_info("Computing all interactions now...")
    cli_alert_info("Using {n_jobs} cores for parallel processing")
    cli_alert_info(
      "This may take 20-40 minutes depending on data size..."
    )

    compute_partial_parallel <- function(cluster_type) {
      cli_alert_info("Creating {cluster_type} cluster")
      cl <- parallel::makeCluster(n_jobs, type = cluster_type)
      on.exit(
        {
          try(parallel::stopCluster(cl), silent = TRUE)
        },
        add = TRUE
      )

      doParallel::registerDoParallel(cl)

      parallel::clusterExport(cl, c("model_cache_file"), envir = environment())
      parallel::clusterEvalQ(cl, {
        suppressPackageStartupMessages({
          library(generics)
          library(workflows)
          library(parsnip)
          library(dbarts)
          library(pdp)
        })

        Sys.setenv(
          OMP_NUM_THREADS = "1",
          OPENBLAS_NUM_THREADS = "1",
          MKL_NUM_THREADS = "1",
          VECLIB_MAXIMUM_THREADS = "1",
          NUMEXPR_NUM_THREADS = "1"
        )

        # Build a per-worker engine. PSOCK cannot safely reuse the master's
        # dbarts engine (external pointers), so we refit inside each process.
        .tm <- readRDS(model_cache_file)
        .train_data_local <- .tm$training_data
        set.seed(102)
        .fit_local <- generics::fit(.tm$model, .train_data_local)
        .bart_engine_local <- workflows::extract_fit_engine(.fit_local)
        NULL
      })

      cli_alert_info("Cluster ready; computing {length(all_pairs)} pairs with pdp::partial(parallel=TRUE)")

      # Capture the flag value in this scope so it survives export to workers.
      match_pdp_default <- isTRUE(PDP_MATCH_PDP_DEFAULT)

      pred_fun_local <- function(object, newdata) {
        # `object` is unused; prediction uses worker-local engine
        # IMPORTANT: `pdp::partial()` expects `pred.fun` to return either
        #   (a) a single number per grid-point (standard PDP), or
        #   (b) a vector per observation (ICE).
        # The default pdp implementation (when pred.fun = NULL) will compute
        # `mean(get_predictions(...))` per grid-point.
        #
        # Resolve the worker-local engine explicitly (avoid relying on the
        # calling environment after export).
        engine <- get(".bart_engine_local", envir = .GlobalEnv)
        p <- stats::predict(engine, newdata = newdata)

        if (isTRUE(match_pdp_default)) {
          # Match pdp's default:
          # get_predictions.default() uses pred[, 1] if predict() returns a matrix,
          # then pardep() takes mean(pred).
          if (is.matrix(p) || is.data.frame(p)) {
            p <- p[, 1L, drop = TRUE]
          }
          return(mean(as.numeric(p), na.rm = TRUE))
        }

        # More standard PDP: average predictions across all observations.
        if (is.matrix(p) || is.data.frame(p)) {
          # Common dbarts shape: ndraws x nobs
          if (ncol(p) == nrow(newdata)) {
            return(mean(colMeans(p), na.rm = TRUE))
          }
          # Alternative shape: nobs x ndraws
          if (nrow(p) == nrow(newdata)) {
            return(mean(rowMeans(p), na.rm = TRUE))
          }
          return(mean(p, na.rm = TRUE))
        }

        mean(as.numeric(p), na.rm = TRUE)
      }

      # First pair as canary
      cli_alert_info("Processing first pair as test...")
      first_part <- pdp::partial(
        object = NULL,
        pred.fun = pred_fun_local,
        pred.var = all_pairs[[1]],
        train = partial_data,
        type = "regression",
        parallel = TRUE,
        ice = FALSE,
        paropts = list(
          .packages = c("generics", "workflows", "parsnip", "dbarts", "pdp"),
          .export = c("pred_fun_local")
        )
      )

      if (stats::sd(first_part$yhat) == 0) {
        cli_alert_warning("First pair failed (sd=0), stopping early")
        return(list(first_part))
      }

      cli_alert_success("First pair succeeded, processing remaining {length(all_pairs) - 1} pairs...")
      remaining_parts <- purrr::map(all_pairs[-1], function(pair) {
        pdp::partial(
          object = NULL,
          pred.fun = pred_fun_local,
          pred.var = pair,
          train = partial_data,
          type = "regression",
          parallel = TRUE,
          ice = FALSE,
          paropts = list(
            .packages = c("generics", "workflows", "parsnip", "dbarts", "pdp"),
            .export = c("pred_fun_local")
          )
        )
      })

      c(list(first_part), remaining_parts)
    }

    # Default: PSOCK everywhere. On Linux, if we hit the pathological case
    # (sd(yhat) == 0), retry with FORK which can behave differently for models
    # backed by compiled code/external pointers.
    parts <- compute_partial_parallel("PSOCK")
    if (stats::sd(parts[[1]]$yhat) == 0 && .Platform$OS.type == "unix") {
      cli_alert_warning("sd(yhat)==0 with PSOCK; retrying with FORK cluster")
      parts <- compute_partial_parallel("FORK")
    }

    Sys.unsetenv("_R_CHECK_LIMIT_CORES_") # restore the 2 cores limit

    if (stats::sd(parts[[1]]$yhat) == 0) {
      cli_abort(
        "Parallel PDP failed. Try setting PARALLEL = FALSE."
      )
    } else {
      cli_alert_success("Parallel PDP computation complete")
    }
  } else {
    cli_alert_info("Using sequential computation (PARALLEL = FALSE)")
    parts <- lapply(all_pairs, function(pair) {
      pdp::partial(
        object = bart_engine,
        pred.var = pair,
        train = partial_data,
        type = "regression",
        parallel = FALSE
      )
    })

    cli_alert_success("Sequential PDP computation complete")
  }

  pdp_elapsed <- difftime(Sys.time(), pdp_start_time, units = "mins")
  cli_alert_success("PDP computation time: {.val {round(as.numeric(pdp_elapsed), 2)}} minutes")

  ints <- purrr::map_vec(parts, function(x) {
    mean(c(
      stats::sd(tapply(x$yhat, INDEX = x[[1]], FUN = stats::sd)),
      stats::sd(tapply(x$yhat, INDEX = x[[2]], FUN = stats::sd))
    ))
  })

  pairs <- purrr::transpose(all_pairs)

  interactions <- tibble::tibble(
    "Variables" = paste0(pairs[[1]], "*", pairs[[2]]),
    "Interaction" = ints
  )

  interactions <- interactions |> dplyr::arrange(desc(Interaction))
  saveRDS(interactions, file = cache_interactions)
  cli_alert_success("Interactions saved to cache")
}

cli_alert_success("Interaction analysis complete")
print(interactions)

if (identical(Sys.getenv("STOP_AFTER_INTERACTIONS"), "1")) {
  cli_alert_info("STOP_AFTER_INTERACTIONS=1 set; exiting early")
  quit(save = "no", status = 0)
}

# =============================================================================
# IMPORTANCE ANALYSIS
# =============================================================================

# PARALLEL <- TRUE # Use parallel processing (20 cores available)

cli_h2("Computing Variable Importance")

cache_importance <- file.path(CACHE_DIR, glue::glue("importances_{DATASET}_{METRIC}.rds"))

if (file.exists(cache_importance)) {
  cli_alert_info("Loading cached importance results...")
  importance_results <- readRDS(cache_importance)
  importance_firm <- importance_results$importance_firm
  importance_perm <- importance_results$importance_perm
  importance_shap <- importance_results$importance_shap
  shap_fastshap_all_test <- importance_results$shap_fastshap_all_test
} else {
  # FIRM
  cli_alert_info("1/3 Computing FIRM importance (ICE curves)...") # nolint nonportable_path_linter
  importance_firm <- check_importance(bart_engine, testing_data, testing_data, predictors_names,
    type = "firm", nsim = NSIM_FIRM, parallel = PARALLEL
  )
  importance_firm_data <- ggplot2::ggplot_build(importance_firm)$plot$data

  # Permutation
  cli_alert_info("2/3 Computing Permutation importance ({NSIM_PERM} iterations)...") # nolint nonportable_path_linter
  cli_alert_info("This is slower than FIRM (may take 40-60 minutes)...")
  importance_perm <- check_importance(bart_engine, testing_data, testing_data, predictors_names,
    type = "permute", nsim = NSIM_PERM, parallel = PARALLEL
  )
  importance_perm_plot_data <- ggplot2::ggplot_build(importance_perm)$plot$data
  importance_perm_raw <- attr(importance_perm_plot_data, "raw_scores")
  importance_perm_data <- tibble::as_tibble(t(importance_perm_raw)) |>
    select(all_of(predictors_names)) |>
    tidyr::pivot_longer(everything(), names_to = "Variable", values_to = "Importance")

  # SHAP
  cli_alert_info("3/3 Computing SHAP importance ({NSIM_SHAP} iterations)...") # nolint nonportable_path_linter
  cli_alert_info("This is the slowest step (may take more than 5 hours)...")

  importance_shap <- check_importance(bart_engine, train_data, testing_data[, predictors_names], predictors_names,
    type = "shap", nsim = NSIM_SHAP, parallel = PARALLEL
  )
  importance_shap_data <- ggplot2::ggplot_build(importance_shap)$plot$data

  # SHAP explanations for dependence plots
  cli_alert_info("Generating SHAP explanations...")
  shap_fastshap_all_test <- shap_explain(bart_engine,
    train_data[, predictors_names],
    testing_data[, predictors_names],
    predictors_names,
    nsim = NSIM_SHAP,
    parallel = PARALLEL
  )

  # Cache results
  importance_results <- list(
    importance_firm = importance_firm_data,
    importance_perm = importance_perm_data,
    importance_shap = importance_shap_data,
    shap_fastshap_all_test = shap_fastshap_all_test
  )

  saveRDS(importance_results, file = cache_importance)
  cli_alert_success("Importance results saved to cache")

  # Reassign for plotting
  importance_firm <- importance_firm_data
  importance_perm <- importance_perm_data
  importance_shap <- importance_shap_data
}

cli_alert_success("Variable importance analysis complete")

# =============================================================================
# VISUALIZATION AND SUMMARY
# =============================================================================
# Visualization and summary code has been extracted to parameter_plots.R
# to allow standalone use in Rmd documents.
#
# To generate plots:
# source(here("scripts", "regime_detection", "evaluation", "parameter_plots.R"))
#
# Required objects must be in the environment:
# - interactions, importance_firm, importance_perm, importance_shap
# - shap_fastshap_all_test, testing_data, tree_data
# - bart_engine, rmse_val, rsq_val
# - DATASET, METRIC, NSIM_FIRM, NSIM_PERM, NSIM_SHAP
# - cache_file, cache_interactions, cache_importance
