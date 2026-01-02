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
DATASET <- "malignantventricular" # Options: malignantventricular, afib_regimes, vtachyarrhythmias
METRIC <- "f3_weighted" # Options: f1_classic, f1_weighted, f3_classic, f3_weighted,
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
CACHE_DIR <- here::here("output", "parameter_analysis_parallel")
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

## best fit model

cache_file <- file.path(CACHE_DIR, glue::glue("bart_bestengine_{DATASET}_{METRIC}.rds"))
cache_file2 <- file.path(CACHE_DIR, glue::glue("bart_bestparsnip_{DATASET}_{METRIC}.rds"))

# Fit final model
# set.seed(102)
# best_fit <- generics::fit(trained_model$model, train_data)

if (file.exists(cache_file) && file.exists(cache_file2)) {
  cli_alert_info("Loading cached best model from {.path {cache_file}}...")
  bart_engine <- readRDS(cache_file)
  bart_parsnip <- readRDS(cache_file2)
} else {
  cli_alert_info("Fitting best model on full training data...")
  set.seed(102)
  best_fit <- generics::fit(trained_model$model, train_data)

  bart_parsnip <- workflows::extract_fit_parsnip(best_fit)
  bart_engine <- workflows::extract_fit_engine(best_fit)

  cli_alert_info("Saving best model to cache...")
  saveRDS(bart_engine, file = cache_file)
  saveRDS(bart_parsnip, file = cache_file2)
  cli_alert_success("Best model saved to {.path {cache_file}}")
}

# Evaluate model performance
# pred <- predict(bart_engine, testing_data)
# pred <- colMeans(pred)

# rmse_val <- yardstick::rmse_vec(testing_data$mean, pred)
# rsq_val <- yardstick::rsq_vec(testing_data$mean, pred)

# cli_alert_success("Model engine RMSE: {.val {round(rmse_val, 4)}}")
# cli_alert_success("Model engine R²: {.val {round(rsq_val, 4)}}")

# if (rmse_val > 0.5 * sd(testing_data$mean)) {
#   cli_alert_warning("RMSE is high relative to data variance. Results may be unreliable.")
# }

# pred2 <- predict(bart_parsnip, testing_data)
# pred2 <- as.numeric(pred2$.pred)

# rmse_val <- yardstick::rmse_vec(testing_data$mean, pred2)
# rsq_val <- yardstick::rsq_vec(testing_data$mean, pred2)

# cli_alert_success("Model parsnip RMSE: {.val {round(rmse_val, 4)}}")
# cli_alert_success("Model parsnip R²: {.val {round(rsq_val, 4)}}")

# if (rmse_val > 0.5 * sd(testing_data$mean)) {
#   cli_alert_warning("RMSE is high relative to data variance. Results may be unreliable.")
# }


# pred3 <- predict(best_fit, testing_data)
# pred3 <- as.numeric(pred3$.pred)

# rmse_val <- yardstick::rmse_vec(testing_data$mean, pred3)
# rsq_val <- yardstick::rsq_vec(testing_data$mean, pred3)

# cli_alert_success("Model parsnip RMSE: {.val {round(rmse_val, 4)}}")
# cli_alert_success("Model parsnip R²: {.val {round(rsq_val, 4)}}")

# if (rmse_val > 0.5 * sd(testing_data$mean)) {
#   cli_alert_warning("RMSE is high relative to data variance. Results may be unreliable.")
# }


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

  all_pairs <- utils::combn(predictors_names, m = 2)
  all_pairs <- purrr::array_tree(all_pairs, 2)

  set.seed(123)
  partial_data <- train_data #|> dplyr::slice_sample(n = 100) # this is for debugging, use full data later

  pdp_start_time <- Sys.time()

  if (PARALLEL) {
    cli_alert_info("Parallel processing enabled for interaction computation")

    n_jobs <- 20 # floor(parallelly::availableCores(methods = "system") / 2)

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
      })

      cli_alert_info("foreach backend workers: {foreach::getDoParWorkers()}")

      parts <- purrr::map(all_pairs, function(x, ...) {
        pdp::partial(pred.var = x, ...)
      },
      object = bart_engine,
      train = partial_data,
      type = "regression",
      parallel = TRUE,
      ice = FALSE,
      paropts = list(
        .packages = c("parsnip", "dbarts", "pdp")
      )
      )
      parts
    }

    # Default: PSOCK everywhere. On Linux, if we hit the pathological case
    # (sd(yhat) == 0), retry with FORK which can behave differently for models
    # backed by compiled code/external pointers.
    parts <- compute_partial_parallel("PSOCK")
    if (stats::sd(parts[[1]]$yhat) == 0 && .Platform$OS.type == "unix") {
      cli_alert_warning("sd(yhat)==0 with PSOCK; retrying with FORK cluster")
      parts <- compute_partial_parallel("FORK")
    }

    # sd(parts[[1]]$yhat)

    # using bart_engine
    # parts <- purrr::map(all_pairs, function(x, ...) {
    #   pdp::partial(pred.var = x, ...)
    # },
    # object = bart_engine, train = partial_data,
    # type = "regression", parallel = TRUE, ice = FALSE
    # )

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
    # parts <- lapply(all_pairs, function(pair) {
    parts <- list()
    parts[[1]] <- pdp::partial(
      object = bart_engine,
      pred.var = predictors_names,
      train = partial_data,
      type = "regression",
      parallel = FALSE
    )
    # })

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
