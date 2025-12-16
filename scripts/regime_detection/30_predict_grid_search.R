# region Generate Predictions with Grid Search
# This script performs exhaustive grid search over all hyperparameters
# Grid: regime_threshold × regime_landmark × min_gap_samples
# Input: tidy_dataset.rds + matrix_profiles_w*.rds (from steps 10, 20)
# Output: predictions_grid.rds with all combinations

suppressPackageStartupMessages({
  library(cli, quietly = TRUE, warn.conflicts = FALSE)
  library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
  library(purrr, quietly = TRUE, warn.conflicts = FALSE)
  library(tibble, quietly = TRUE, warn.conflicts = FALSE)
  library(here, quietly = TRUE, warn.conflicts = FALSE)
  library(furrr, quietly = TRUE, warn.conflicts = FALSE)
  library(future, quietly = TRUE, warn.conflicts = FALSE)
})

# Load FLOSS functions
source(here::here("scripts", "helpers", "glue_fmt.R"), local = .GlobalEnv, encoding = "UTF-8")
source(here::here("scripts", "helpers", "predict_floss_changes.R"), local = .GlobalEnv, encoding = "UTF-8")
source(here::here("scripts", "common", "read_ecg.R"), local = .GlobalEnv, encoding = "UTF-8")

# region Configuration
# ===== DATASET SELECTION =====
# Must match dataset from scripts 10 and 20
# CLI override: Rscript 30_predict_grid_search.R <dataname>
default_dataname <- "malignantventricular"
cli_args <- commandArgs(trailingOnly = TRUE)
dataname <- if (length(cli_args) >= 1L && nzchar(cli_args[1L])) cli_args[1L] else default_dataname
cli::cli_alert_info("Dataset selected: {dataname}")

const_sample_freq <- 250

# Grid search hyperparameters
# regime_threshold: sensitivity for detecting regime changes (18 values: 0.05 to 0.9)
var_regime_threshold <- seq(0.05, 0.9, by = 0.05)

# regime_landmark: temporal lag in seconds where threshold is applied (15 values: 2s to 9s)
# A landmark of 2 means detection occurs 2 seconds behind streaming position
var_regime_landmark <- seq(2, 9, by = 0.5)

# min_gap_samples: minimum distance between consecutive predictions (6 values)
# This is the parameter used in clean_pred() to remove duplicates
# Values: 200 (0.8s), 500 (2s), 1000 (4s), 2000 (8s), 3000 (12s), 5000 (20s) at 250Hz
var_min_gap_samples <- c(200, 500, 1000, 2000, 3000, 5000)

# Window sizes (must match generated matrix profiles)
var_window_size <- seq(25, 400, by = 25)

# Input/Output paths
input_dir <- here("output", "regime_detection", dataname, "generation")
output_dir <- here("output", "regime_detection", dataname, "prediction")
tidy_file <- file.path(input_dir, "tidy_dataset.rds")
output_file <- file.path(output_dir, "predictions_grid.rds")
intermediate_dir <- file.path(output_dir, "intermediate_predictions")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(intermediate_dir, recursive = TRUE, showWarnings = FALSE)

# Memory management - process in batches
batch_size <- 4 # Process 4 window sizes at a time to manage memory

# Parallel processing
n_workers <- 20
use_parallel <- n_workers > 1
# endregion Configuration

extract_window_id <- function(path) {
  as.integer(gsub("[^0-9]", "", basename(path)))
}

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

# Validate required matrix profiles
expected_mp_files <- file.path(input_dir, glue::glue("matrix_profiles_w{var_window_size}.rds"))
missing_mp <- expected_mp_files[!file.exists(expected_mp_files)]
if (length(missing_mp) > 0) {
  cli::cli_abort(c(
    "x" = "Missing matrix profile files for {length(missing_mp)} window sizes.",
    "i" = paste("First missing file:", missing_mp[[1]])
  ))
}
# endregion Step 1

# region Step 2 - Create Grid
cli::cli_h2("Step 2: Creating hyperparameter grid")
# Grid without min_gap_samples (applied later after raw predictions)
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
parallel_desc <- if (use_parallel) as.character(n_workers) else "1 (sequential)"
cli::cli_inform(c("i" = "Parallel workers: {parallel_desc}"))

# Set up parallel processing
if (use_parallel) {
  future::plan(future::multicore, workers = n_workers)
} else {
  cli::cli_alert_info("n_workers == 1, running sequentially without futures")
}

prediction_counter <- 0

total_tic <- Sys.time()

# Identify which windows still need predictions
existing_intermediate <- list.files(intermediate_dir,
  pattern = "predictions_w.*\\.rds$",
  full.names = TRUE
)
windows_done <- sort(extract_window_id(existing_intermediate))
missing_windows <- setdiff(var_window_size, windows_done)

if (length(missing_windows) == 0) {
  if (file.exists(output_file)) {
    cli::cli_alert_info("All intermediate and final outputs already exist, skipping grid search.")
    quit(status = 0)
  }
  cli::cli_alert_info("All intermediate files exist; skipping generation and proceeding to combine.")
}

# Process only missing window sizes in batches
if (length(missing_windows) > 0) {
  window_batches <- split(missing_windows, ceiling(seq_along(missing_windows) / batch_size))
} else {
  window_batches <- list()
}

for (batch_idx in seq_along(window_batches)) {
  batch_windows <- window_batches[[batch_idx]]
  cli::cli_alert_info("Batch {batch_idx}/{length(window_batches)}: Windows {paste(batch_windows, collapse = ', ')}")

  batch_tic <- Sys.time()

  for (w in batch_windows) {
    window_file <- file.path(intermediate_dir, glue::glue("predictions_w{w}.rds"))
    if (file.exists(window_file)) {
      cli::cli_alert_info("Window {w}: Intermediate file already exists, skipping")
      next
    }

    # Load Matrix Profile for this window size
    mp_file <- file.path(input_dir, glue::glue("matrix_profiles_w{w}.rds"))
    if (!file.exists(mp_file)) {
      cli::cli_alert_warning("Window {w}: Matrix Profile not found, skipping")
      next
    }

    mp_dataset <- readRDS(mp_file)
    cli::cli_inform(c("i" = "Window {w}: Loaded {nrow(mp_dataset)} Matrix Profiles"))

    # Extract only the necessary components to minimize globals size
    records_vec <- mp_dataset$record
    floss_list <- mp_dataset$floss
    truth_list <- tidy_dataset$truth
    truth_records <- tidy_dataset$record

    # Process all records in parallel (pass only small vectors/lists as arguments)
    map_fun <- function(record_idx,
                        records_vec,
                        floss_list,
                        truth_list,
                        truth_records,
                        base_grid,
                        var_min_gap_samples,
                        w) {
      record_id <- records_vec[record_idx]
      floss_obj <- floss_list[[record_idx]]
      truth <- truth_list[[which(truth_records == record_id)]]

      record_results <- list()
      counter <- 0

      # Apply all threshold × landmark combinations
      for (grid_idx in seq_len(nrow(base_grid))) {
        rt <- base_grid$regime_threshold[grid_idx]
        rl <- base_grid$regime_landmark[grid_idx]

        # Generate RAW predictions (without clean_pred)
        raw_pred <- floss_predict(floss_obj, w, 0, rt, rl)

        # Now apply each min_gap_samples value
        for (min_gap in var_min_gap_samples) {
          # Apply clean_pred with this specific min_gap_samples
          # Keeps first detection within each gap (timeout behavior)
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
    }

    if (use_parallel) {
      batch_predictions <- furrr::future_map(
        seq_len(nrow(mp_dataset)),
        map_fun,
        records_vec = records_vec,
        floss_list = floss_list,
        truth_list = truth_list,
        truth_records = truth_records,
        base_grid = base_grid,
        var_min_gap_samples = var_min_gap_samples,
        w = w,
        .options = furrr::furrr_options(seed = NULL)
      )
    } else {
      batch_predictions <- purrr::map(
        seq_len(nrow(mp_dataset)),
        map_fun,
        records_vec = records_vec,
        floss_list = floss_list,
        truth_list = truth_list,
        truth_records = truth_records,
        base_grid = base_grid,
        var_min_gap_samples = var_min_gap_samples,
        w = w
      )
    }

    # Combine all record results for this window
    window_predictions <- dplyr::bind_rows(batch_predictions)
    prediction_counter <- prediction_counter + nrow(window_predictions)
    saveRDS(window_predictions, file = window_file, compress = "xz")
    cli::cli_alert_success("Window {w}: Saved {nrow(window_predictions)} rows to {window_file}")

    # Free memory after persisting this window
    rm(window_predictions, batch_predictions)
    gc()

    # Free memory after each window size
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

rm(tidy_dataset)
gc()

# region Step 4 - Combine and Save
cli::cli_h2("Step 4: Combining predictions")
intermediate_files <- list.files(intermediate_dir,
  pattern = "predictions_w.*\\.rds$",
  full.names = TRUE
)

if (length(intermediate_files) == 0) {
  cli::cli_abort(c(
    "x" = "Nenhum ficheiro intermédio encontrado em {intermediate_dir}",
    "i" = "Execute novamente o processamento para gerar os resultados por janela"
  ))
}

intermediate_files <- intermediate_files[order(vapply(
  intermediate_files,
  extract_window_id,
  numeric(1)
))]
rm(extract_window_id)

cli::cli_inform(c("i" = "Ficheiros intermédios encontrados: {length(intermediate_files)}"))
predictions_grid <- purrr::map_dfr(intermediate_files, readRDS)
cli::cli_inform(c("i" = "Total rows: {nrow(predictions_grid)}"))

# Format hyperparameters
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
