# region Generate Predictions with Grid Search
# Comment: This script performs exhaustive grid search over all hyperparameters
# Comment: Grid: regime_threshold × regime_landmark × min_gap_samples
# Comment: Input: tidy_dataset.rds + matrix_profiles_w*.rds (from steps 10, 20)
# Comment: Output: predictions_grid.rds with all combinations

library(cli)
library(dplyr)
library(purrr)
library(tibble)
library(here)
library(furrr)
library(future)

# Comment: Load FLOSS functions
source(here::here("scripts", "helpers", "glue_fmt.R"), local = .GlobalEnv, encoding = "UTF-8")
source(here::here("R", "floss_predict.R"), local = .GlobalEnv, encoding = "UTF-8")

# Comment: Load clean_pred function
script_files <- list.files(here::here("scripts", "common"), pattern = "*.R")
purrr::walk(here::here("scripts", "common", script_files), source, local = .GlobalEnv, encoding = "UTF-8")
rm(script_files)

# region Configuration
# Comment: ===== DATASET SELECTION =====
# Comment: Must match dataset from scripts 10 and 20

dataname <- "afib_regimes"
# dataname <- "vtachyarrhythmias"
# dataname <- "malignantventricular"

const_sample_freq <- 250

# Comment: Grid search hyperparameters
# Comment: regime_threshold: sensitivity for detecting regime changes (18 values: 0.05 to 0.9)
var_regime_threshold <- seq(0.05, 0.9, by = 0.05)

# Comment: regime_landmark: temporal lag in seconds where threshold is applied (15 values: 2s to 9s)
# Comment: A landmark of 2 means detection occurs 2 seconds behind streaming position
var_regime_landmark <- seq(2, 9, by = 0.5)

# Comment: min_gap_samples: minimum distance between consecutive predictions (6 values)
# Comment: This is the parameter used in clean_pred() to remove duplicates
# Comment: Values: 200 (0.8s), 500 (2s), 1000 (4s), 2000 (8s), 3000 (12s), 5000 (20s) at 250Hz
var_min_gap_samples <- c(200, 500, 1000, 2000, 3000, 5000)

# Comment: Window sizes (must match generated matrix profiles)
var_window_size <- seq(25, 400, by = 25)

# Comment: Input/Output paths
input_dir <- here("output", "regime_detection", dataname, "generation")
output_dir <- here("output", "regime_detection", dataname, "prediction")
tidy_file <- file.path(input_dir, "tidy_dataset.rds")
output_file <- file.path(output_dir, "predictions_grid.rds")

# Comment: Memory management - process in batches
batch_size <- 4 # Comment: Process 4 window sizes at a time to manage memory

# Comment: Parallel processing
n_workers <- 20
# endregion Configuration

cli::cli_h1("Regime Detection - Prediction Grid Search")
cli::cli_inform(c("i" = "Dataset: {dataname}"))
cli::cli_inform(c("i" = "Grid dimensions:"))
cli::cli_inform(c("*" = "  Window sizes: {length(var_window_size)}"))
cli::cli_inform(c("*" = "  Regime thresholds: {length(var_regime_threshold)}"))
cli::cli_inform(c("*" = "  Regime landmarks: {length(var_regime_landmark)}"))
cli::cli_inform(c("*" = "  Min gap samples: {length(var_min_gap_samples)}"))
total_combinations <- length(var_window_size) * length(var_regime_threshold) *
  length(var_regime_landmark) * length(var_min_gap_samples)
cli::cli_inform(c("!" = "Total combinations per record: {total_combinations}"))

# region Step 1 - Load Tidy Dataset
cli::cli_h2("Step 1: Loading tidy dataset")
if (!file.exists(tidy_file)) {
  cli::cli_abort(c(
    "x" = "Tidy dataset not found: {tidy_file}",
    "i" = "Run 10_prepare_data.R first"
  ))
}

tidy_dataset <- readRDS(tidy_file)
cli::cli_inform(c("v" = "Loaded {nrow(tidy_dataset)} records"))
# endregion Step 1

# region Step 2 - Create Grid
cli::cli_h2("Step 2: Creating hyperparameter grid")
# Comment: Grid without min_gap_samples (applied later after raw predictions)
base_grid <- expand.grid(
  regime_threshold = var_regime_threshold,
  regime_landmark = var_regime_landmark,
  stringsAsFactors = FALSE
)
cli::cli_inform(c("i" = "Base grid size: {nrow(base_grid)} combinations"))
cli::cli_inform(c("i" = "Will apply {length(var_min_gap_samples)} min_gap_samples values after prediction"))
# endregion Step 2

# region Step 3 - Generate Predictions
cli::cli_h2("Step 3: Generating predictions")
cli::cli_inform(c("i" = "Processing window sizes in batches of {batch_size}"))
cli::cli_inform(c("i" = "Parallel workers: {n_workers}"))

# Comment: Set up parallel processing
future::plan(future::multicore, workers = n_workers)

all_predictions <- list()
prediction_counter <- 0

total_tic <- Sys.time()

# Comment: Process window sizes in batches
window_batches <- split(var_window_size, ceiling(seq_along(var_window_size) / batch_size))

for (batch_idx in seq_along(window_batches)) {
  batch_windows <- window_batches[[batch_idx]]
  cli::cli_alert_info("Batch {batch_idx}/{length(window_batches)}: Windows {paste(batch_windows, collapse = ', ')}")

  batch_tic <- Sys.time()

  for (w in batch_windows) {
    # Comment: Load Matrix Profile for this window size
    mp_file <- file.path(input_dir, glue::glue("matrix_profiles_w{w}.rds"))
    if (!file.exists(mp_file)) {
      cli::cli_alert_warning("Window {w}: Matrix Profile not found, skipping")
      next
    }

    mp_dataset <- readRDS(mp_file)
    cli::cli_inform(c("i" = "Window {w}: Loaded {nrow(mp_dataset)} Matrix Profiles"))

    # Comment: Process all records in parallel
    batch_predictions <- furrr::future_map(seq_len(nrow(mp_dataset)), function(record_idx) {
      record_id <- mp_dataset$record[record_idx]
      floss_obj <- mp_dataset$floss[[record_idx]]
      truth <- tidy_dataset$truth[[which(tidy_dataset$record == record_id)]]

      record_results <- list()
      counter <- 0

      # Comment: Apply all threshold × landmark combinations
      for (grid_idx in seq_len(nrow(base_grid))) {
        rt <- base_grid$regime_threshold[grid_idx]
        rl <- base_grid$regime_landmark[grid_idx]

        # Comment: Generate RAW predictions (without clean_pred)
        raw_pred <- floss_predict(floss_obj, w, 0, rt, rl)

        # Comment: Now apply each min_gap_samples value
        for (min_gap in var_min_gap_samples) {
          # Comment: Apply clean_pred with this specific min_gap_samples
          # Comment: Keeps first detection within each gap (timeout behavior)
          cleaned_pred <- clean_pred(raw_pred, min_gap)

          counter <- counter + 1
          record_results[[counter]] <- tibble::tibble(
            record = record_id,
            window_size = w,
            regime_threshold = rt,
            regime_landmark = rl,
            min_gap_samples = min_gap,
            truth = list(truth),
            pred = list(cleaned_pred)
          )
        }
      }

      dplyr::bind_rows(record_results)
    }, .options = furrr::furrr_options(seed = NULL))

    # Comment: Combine all record results
    for (pred_df in batch_predictions) {
      for (i in seq_len(nrow(pred_df))) {
        prediction_counter <- prediction_counter + 1
        all_predictions[[prediction_counter]] <- pred_df[i, ]
      }
    }

    # Comment: Free memory after each window size
    rm(mp_dataset)
    gc()
  }

  batch_tac <- Sys.time()
  batch_elapsed <- round(difftime(batch_tac, batch_tic, units = "mins"), 2)
  cli::cli_alert_success("Batch {batch_idx} complete in {batch_elapsed} mins")
  cli::cli_inform(c("i" = "Total predictions so far: {prediction_counter}"))
}

total_tac <- Sys.time()
total_elapsed <- round(difftime(total_tac, total_tic, units = "mins"), 2)
# endregion Step 3

# region Step 4 - Combine and Save
cli::cli_h2("Step 4: Combining predictions")
predictions_grid <- dplyr::bind_rows(all_predictions)
cli::cli_inform(c("i" = "Total rows: {nrow(predictions_grid)}"))

# Comment: Format hyperparameters
predictions_grid <- predictions_grid |>
  dplyr::mutate(
    regime_threshold = round(regime_threshold, 2),
    regime_landmark = round(regime_landmark, 1),
    window_size = as.integer(window_size),
    min_gap_samples = as.integer(min_gap_samples)
  )

cli::cli_h2("Step 5: Saving predictions grid")
saveRDS(predictions_grid, file = output_file, compress = "xz")
cli::cli_inform(c("v" = "Saved to: {output_file}"))
file_size_mb <- round(file.size(output_file) / 1024 / 1024, 2)
cli::cli_inform(c("*" = "File size: {file_size_mb} MB"))
cli::cli_inform(c("*" = "Rows: {nrow(predictions_grid)}"))
cli::cli_inform(c("*" = "Columns: {paste(names(predictions_grid), collapse = ', ')}"))
cli::cli_inform(c("!" = "Total time: {total_elapsed} minutes"))
# endregion Step 4

# region Step 5 - Summary Statistics
cli::cli_h2("Summary")
cli::cli_inform(c("i" = "Unique values:"))
cli::cli_inform(c("*" = "  Records: {length(unique(predictions_grid$record))}"))
cli::cli_inform(c("*" = "  Window sizes: {length(unique(predictions_grid$window_size))}"))
cli::cli_inform(c("*" = "  Regime thresholds: {length(unique(predictions_grid$regime_threshold))}"))
cli::cli_inform(c("*" = "  Regime landmarks: {length(unique(predictions_grid$regime_landmark))}"))
cli::cli_inform(c("*" = "  Min gap samples: {length(unique(predictions_grid$min_gap_samples))}"))

expected_rows <- length(unique(predictions_grid$record)) * total_combinations
cli::cli_inform(c("!" = "Expected rows: {expected_rows}"))
cli::cli_inform(c("!" = "Actual rows: {nrow(predictions_grid)}"))

if (nrow(predictions_grid) == expected_rows) {
  cli::cli_alert_success("All combinations generated successfully!")
} else {
  cli::cli_alert_warning("Row count mismatch - some combinations may be missing")
}
# endregion Step 5
# endregion Generate Predictions
