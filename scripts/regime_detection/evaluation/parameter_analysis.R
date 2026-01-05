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
DATASET <- "vtachyarrhythmias" # Options: malignantventricular, afib_regimes, vtachyarrhythmias
METRIC <- "precision_10s" # Options: f1_classic, f1_weighted, f3_classic, f3_weighted,
#          recall_4s, recall_10s, precision_4s, precision_10s,
#          edd_median_s, fp_per_min,
#          nab_score_standard, nab_score_low_fp, nab_score_low_fn

PARALLEL <- TRUE # Use parallel processing (20 cores available)
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
cli_alert_info("Preparing aggregated dataset...")
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

# Fit final model
cli_alert_info("Fitting best model on full training data...")
set.seed(102)
best_fit <- generics::fit(trained_model$model, train_data)
bart_engine <- workflows::extract_fit_engine(best_fit)

## best fit model

# cache_file <- file.path(CACHE_DIR, glue::glue("bart_bestengine_{DATASET}_{METRIC}.rds"))

# Fit final model
# if (file.exists(cache_file)) {
#   cli_alert_info("Loading cached best model from {.path {cache_file}}...")
#   bart_engine <- readRDS(cache_file)
# } else {
#   cli_alert_info("Fitting best model on full training data...")
#   set.seed(102)
#   best_fit <- generics::fit(trained_model$model, train_data)

#   bart_engine <- workflows::extract_fit_engine(best_fit)

#   cli_alert_info("Saving best model to cache...")
#   saveRDS(bart_engine, file = cache_file)
#   cli_alert_success("Best model saved to {.path {cache_file}}")
# }

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

  # NOTE: In some setups (esp. containers), exporting a fitted BART engine
  # (dbarts) to parallel workers can be unstable because the object contains
  # compiled-code state/external pointers. A robust workaround is to cache the
  # engine to disk and have each worker lazily load it once.
  bart_engine_cache <- file.path(
    CACHE_DIR,
    glue::glue("bart_engine_{DATASET}_{METRIC}.rds")
  )
  if (!file.exists(bart_engine_cache)) {
    saveRDS(bart_engine, bart_engine_cache)
  }

  pdp_pred_fun <- local({
    engine <- NULL
    function(object, newdata) {
      if (is.null(engine)) {
        engine <<- readRDS(object)
      }

      p <- stats::predict(engine, newdata)

      if (is.matrix(p)) {
        # dbarts can return draws as (ndraws x nobs) or (nobs x ndraws)
        if (nrow(p) == nrow(newdata)) {
          return(rowMeans(p))
        }
        if (ncol(p) == nrow(newdata)) {
          return(colMeans(p))
        }
      }

      as.numeric(p)
    }
  })

  all_pairs <- utils::combn(predictors_names, m = 2)
  all_pairs <- purrr::array_tree(all_pairs, 2)

  set.seed(123)
  partial_data <- train_data #|> dplyr::slice_sample(n = 100) # this is for debugging, use full data later

  pdp_start_time <- Sys.time()

  if (PARALLEL) {
    cli_alert_info("Parallel processing enabled for interaction computation")

    system_cores <- parallelly::availableCores(methods = "system")
    cgroup_cores <- tryCatch(
      parallelly::availableCores(methods = "cgroups2"),
      error = function(e) NA_integer_
    )
    effective_cores <- if (is.na(cgroup_cores)) system_cores else min(system_cores, cgroup_cores)
    n_jobs <- max(1, effective_cores - 2)

    Sys.setenv("_R_CHECK_LIMIT_CORES_" = FALSE) # this is needed to use more than 2 cores

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

      parallel::clusterEvalQ(cl, { # ensure predict methods are registered on workers
        library(parsnip)
        library(dbarts)
        library(pdp)

        # Reduce the chance of oversubscription/nested-parallel issues
        # that can cause flaky behaviour in compiled code.
        Sys.setenv(
          OMP_NUM_THREADS = "1",
          OPENBLAS_NUM_THREADS = "1",
          MKL_NUM_THREADS = "1",
          VECLIB_MAXIMUM_THREADS = "1",
          NUMEXPR_NUM_THREADS = "1"
        )
      })

      cli_alert_info("foreach backend workers: {foreach::getDoParWorkers()}")

      diag_parallel_state <- function(reason, first_part = NULL) {
        diag_file <- file.path(
          CACHE_DIR,
          glue::glue("pdp_diag_{DATASET}_{METRIC}_{cluster_type}_{format(Sys.time(), '%Y%m%d_%H%M%S')}.txt")
        )

        # Small, deterministic newdata with variation to test predict() behaviour
        newdata_test <- partial_data
        newdata_test <- newdata_test[, predictors_names, drop = FALSE]
        if (nrow(newdata_test) > 6) {
          newdata_test <- newdata_test[seq_len(6), , drop = FALSE]
        }

        # Force variation even if first rows are identical
        for (j in seq_along(predictors_names)) {
          col <- predictors_names[[j]]
          if (!is.numeric(newdata_test[[col]])) {
            suppressWarnings(newdata_test[[col]] <- as.numeric(newdata_test[[col]]))
          }
          if (nrow(newdata_test) >= 2 && is.finite(newdata_test[[col]][[1]])) {
            newdata_test[[col]][[2]] <- newdata_test[[col]][[1]] + 1
          }
        }

        master_pred <- try(pdp_pred_fun(bart_engine_cache, newdata_test), silent = TRUE)
        master_stats <- list(
          ok = !inherits(master_pred, "try-error"),
          length = if (!inherits(master_pred, "try-error")) length(master_pred) else NA_integer_,
          sd = if (!inherits(master_pred, "try-error")) stats::sd(master_pred) else NA_real_,
          range = if (!inherits(master_pred, "try-error")) range(master_pred) else c(NA_real_, NA_real_)
        )

        worker_fun <- function(engine_path, newdata) {
          suppressPackageStartupMessages({
            library(dbarts)
            library(parsnip)
          })
          Sys.setenv(
            OMP_NUM_THREADS = "1",
            OPENBLAS_NUM_THREADS = "1",
            MKL_NUM_THREADS = "1",
            VECLIB_MAXIMUM_THREADS = "1",
            NUMEXPR_NUM_THREADS = "1"
          )

          eng <- readRDS(engine_path)
          p <- stats::predict(eng, newdata)
          collapsed <- NULL
          if (is.matrix(p)) {
            if (nrow(p) == nrow(newdata)) {
              collapsed <- rowMeans(p)
            } else if (ncol(p) == nrow(newdata)) {
              collapsed <- colMeans(p)
            }
          }
          if (is.null(collapsed)) {
            collapsed <- as.numeric(p)
          }
          list(
            pid = Sys.getpid(),
            r_version = R.version.string,
            platform = R.version$platform,
            predict_dim = if (is.matrix(p)) paste(dim(p), collapse = "x") else "not-matrix",
            pred_length = length(collapsed),
            pred_sd = stats::sd(collapsed),
            pred_range = range(collapsed),
            env_threads = Sys.getenv(c("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS"), unset = NA),
            loaded = sort(loadedNamespaces())
          )
        }

        worker_stats <- try(parallel::clusterCall(cl, worker_fun, bart_engine_cache, newdata_test), silent = TRUE)

        txt <- c(
          "# PDP parallel diagnostic",
          glue::glue("time: {Sys.time()}"),
          glue::glue("dataset: {DATASET}"),
          glue::glue("metric: {METRIC}"),
          glue::glue("cluster_type: {cluster_type}"),
          glue::glue("n_jobs: {n_jobs}"),
          glue::glue("reason: {reason}"),
          "",
          "## Master",
          glue::glue("R: {R.version.string}"),
          glue::glue("platform: {R.version$platform}"),
          glue::glue("availableCores(system): {parallelly::availableCores(methods = 'system')}"),
          glue::glue("master_pred ok: {master_stats$ok}"),
          glue::glue("master_pred length: {master_stats$length}"),
          glue::glue("master_pred sd: {master_stats$sd}"),
          glue::glue("master_pred range: {paste(master_stats$range, collapse = ' .. ')}"),
          glue::glue("env threads: {paste(Sys.getenv(c('OMP_NUM_THREADS','OPENBLAS_NUM_THREADS','MKL_NUM_THREADS'), unset = NA), collapse = ', ')}"),
          "",
          "## pdp::partial first_part",
          glue::glue("first_part exists: {!is.null(first_part)}"),
          if (!is.null(first_part)) glue::glue("first_part yhat sd: {stats::sd(first_part$yhat)}") else "",
          if (!is.null(first_part)) glue::glue("first_part yhat range: {paste(range(first_part$yhat), collapse = ' .. ')}") else "",
          "",
          "## Workers",
          if (inherits(worker_stats, "try-error")) {
            paste0("clusterCall failed: ", as.character(worker_stats))
          } else {
            paste(capture.output(str(worker_stats, max.level = 2)), collapse = "\n")
          },
          "",
          "## sessionInfo(master)",
          paste(capture.output(sessionInfo()), collapse = "\n")
        )

        writeLines(txt, diag_file)
        cli_alert_warning("Wrote PDP diagnostic log to {.path {diag_file}}")
        invisible(diag_file)
      }

      # Process first pair to detect early failures
      cli_alert_info("Processing first pair as test...")
      first_part <- pdp::partial(
        object = bart_engine_cache,
        pred.fun = pdp_pred_fun,
        pred.var = all_pairs[[1]],
        train = partial_data,
        type = "regression",
        parallel = TRUE,
        ice = FALSE,
        paropts = list(
          .packages = c("parsnip", "dbarts", "pdp")
        )
      )

      # Check if first result is valid
      if (stats::sd(first_part$yhat) == 0) {
        cli_alert_warning("First pair failed (sd=0), stopping early")
        diag_parallel_state("sd(yhat)==0 on first pair", first_part)
        return(list(first_part)) # Return early with single item
      }

      # If first succeeded, process remaining pairs
      cli_alert_success("First pair succeeded, processing remaining {length(all_pairs) - 1} pairs...")
      remaining_parts <- purrr::map(all_pairs[-1], function(x, ...) {
        pdp::partial(pred.var = x, ...)
      },
      object = bart_engine_cache,
      pred.fun = pdp_pred_fun,
      train = partial_data,
      type = "regression",
      parallel = TRUE,
      ice = FALSE,
      paropts = list(
        .packages = c("parsnip", "dbarts", "pdp")
      )
      )

      # Combine first + remaining
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

    # print(parts)
    # [[1]]
    #     window_size regime_threshold          yhat
    # 1            25             0.05  2.888514e-03
    # 2            50             0.05  3.250277e-03
    # 3            75             0.05  6.816440e-04
    # 4           100             0.05  1.989367e-03

    # print(interactions)
    # # A tibble: 1 × 2
    #   Variables                    Interaction
    #   <chr>                              <dbl>
    # 1 window_size*regime_threshold     0.00745

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
# VISUALIZATION
# =============================================================================

cli_h2("Generating Plots")

# Plot 1: Interactions
cli_alert_info("Creating interaction plot...")
interactions_plot <- ggplot2::ggplot(interactions, ggplot2::aes(
  x = reorder(Variables, Interaction),
  y = Interaction, fill = Variables
)) +
  ggplot2::geom_col(color = "grey35", linewidth = 0.2) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = glue::glue("Parameter Interactions ({DATASET})"),
    subtitle = glue::glue("Metric: {METRIC}"),
    y = "Interaction Strength",
    x = NULL
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none")

print(interactions_plot)

# Plot 2: Importance Comparison (3 methods side-by-side)
cli_alert_info("Creating importance comparison plot...")

importance_firm_plot <- ggplot2::ggplot(importance_firm, aes(
  x = reorder(Variable, Importance),
  y = Importance, fill = Variable
)) +
  ggplot2::geom_col(colour = "grey35", linewidth = 0.8, show.legend = FALSE) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "FIRM",
    subtitle = "ICE curves",
    x = NULL, y = NULL
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(plot.margin = margin(5, 5, 5, 10))

importance_perm_plot <- ggplot2::ggplot(importance_perm, aes(
  x = reorder(Variable, Importance, FUN = median),
  y = Importance, fill = Variable
)) +
  ggplot2::geom_boxplot(colour = "grey35", linewidth = 0.5, show.legend = FALSE) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Permutation",
    subtitle = glue::glue("{NSIM_PERM} iterations"),
    x = NULL, y = NULL
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(plot.margin = margin(5, 5, 5, 10))

importance_shap_plot <- ggplot2::ggplot(importance_shap, aes(
  x = reorder(Variable, Importance),
  y = Importance, fill = Variable
)) +
  ggplot2::geom_col(colour = "grey35", linewidth = 0.8, show.legend = FALSE) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "SHAP",
    subtitle = glue::glue("{NSIM_SHAP} iterations"),
    x = NULL, y = NULL
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(plot.margin = margin(5, 5, 5, 10))

all_importance <- (importance_firm_plot + plot_layout(tag_level = "keep")) |
  (importance_perm_plot + plot_layout(tag_level = "keep")) |
  (importance_shap_plot + plot_layout(tag_level = "keep")) +
    plot_layout(guides = "collect")

all_importance <- all_importance + plot_annotation(
  title = glue::glue("Variable Importance ({DATASET} - {METRIC})"),
  tag_levels = "A",
  theme = ggplot2::theme_bw() + ggplot2::theme(plot.title = ggplot2::element_text(size = 16))
)

print(all_importance)

# Plot 3: SHAP Dependence Plots (2×2 grid for 4 parameters)
cli_alert_info("Creating SHAP dependence plots...")

d1 <- shapviz::shapviz(shap_fastshap_all_test,
  X = testing_data[, predictors_names],
  baseline = mean(testing_data$mean)
)

shap_plots <- list()
for (feat in predictors_names) {
  shap_plots[[feat]] <- shapviz::sv_dependence(d1, feat, color_var = "auto") +
    ggplot2::geom_smooth(method = "loess", colour = "#0000ff44", alpha = 0.2, se = FALSE) +
    ggplot2::labs(y = "SHAP value", title = feat) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none")
}

all_shap <- wrap_plots(shap_plots, ncol = 2) +
  plot_annotation(
    title = glue::glue("SHAP Dependence Plots ({DATASET} - {METRIC})"),
    subtitle = "Showing how each parameter affects model predictions",
    theme = ggplot2::theme_bw()
  )

print(all_shap)

# =============================================================================
# SUMMARY
# =============================================================================

cli_h2("Analysis Summary")

cli_alert_success("Dataset: {.val {DATASET}}")
cli_alert_success("Metric: {.val {METRIC}}")
cli_alert_success("Configurations analyzed: {.val {nrow(tree_data)}}")
cli_alert_success("Model RMSE: {.val {round(rmse_val, 4)}}")
cli_alert_success("Model R²: {.val {round(rsq_val, 4)}}")

cli_h3("Top 3 Most Important Parameters (SHAP)")
top_params <- importance_shap |>
  arrange(desc(Importance)) |>
  slice_head(n = 3) |>
  pull(Variable)

for (i in seq_along(top_params)) {
  cli_alert_info("{i}. {.field {top_params[i]}}")
}

cli_h3("Strongest Interactions")
top_interactions <- interactions |>
  slice_head(n = 3)

for (i in seq_len(nrow(top_interactions))) {
  cli_alert_info("{i}. {.field {top_interactions$Variables[i]}} (strength: {round(top_interactions$Interaction[i], 3)})")
}

cli_h3("Cache Files")
cli_alert_info("Model: {.path {cache_file}}")
cli_alert_info("Interactions: {.path {cache_interactions}}")
cli_alert_info("Importances: {.path {cache_importance}}")

cli_alert_success("Analysis complete! Plots displayed above.")
cli_alert_info("To analyze another metric, change METRIC variable and re-run.")
