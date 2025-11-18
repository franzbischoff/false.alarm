# region Convert FLOSS Results to CSV Format
# Comment: This script converts FLOSS results to Python-compatible CSV format
# Comment: Reads from new modular pipeline structure
# Comment: Input: predictions_grid.rds (from step 30)
# Comment: Output: {dataname}_predictions.csv

library(dplyr)
library(purrr)
library(readr)
library(jsonlite)
library(here)
library(glue)
library(cli)
library(tibble)

# region Configuration
# Comment: ===== DATASET SELECTION =====
# Comment: Must match dataset from scripts 10, 20, and 30

dataname <- "afib_regimes"
# dataname <- "vtachyarrhythmias"
# dataname <- "malignantventricular"

const_sample_freq <- 250 # Hz

# Comment: Input/Output paths
input_dir <- here("output", "regime_detection", dataname, "prediction")
generation_dir <- here("output", "regime_detection", dataname, "generation")
output_dir <- here("output", "regime_detection", dataname)
input_file <- file.path(input_dir, "predictions_grid.rds")
tidy_file <- file.path(generation_dir, "tidy_dataset.rds")
output_file <- file.path(output_dir, glue("{dataname}_predictions.csv"))

# Comment: Test mode configuration
test_mode <- FALSE # Set to TRUE to export sample, FALSE for full dataset
test_rows <- 1000
# endregion Configuration

cli::cli_h1("Convert FLOSS Results to CSV")
cli::cli_inform(c("i" = "Dataset: {dataname}"))
cli::cli_inform(c("i" = "Input: {input_file}"))
cli::cli_inform(c("i" = "Output: {output_file}"))

# region Step 1 - Load Data
cli::cli_h2("Step 1: Loading predictions and metadata")
if (!file.exists(input_file)) {
  cli::cli_abort(c(
    "x" = "Predictions file not found: {input_file}",
    "i" = "Run 30_predict_grid_search.R first"
  ))
}

if (!file.exists(tidy_file)) {
  cli::cli_abort(c(
    "x" = "Tidy dataset not found: {tidy_file}",
    "i" = "Run 10_prepare_data.R first"
  ))
}

data <- readRDS(input_file)
cli::cli_inform(c("v" = "Loaded {nrow(data)} predictions"))

# Comment: Load signal lengths from tidy dataset
tidy_dataset <- readRDS(tidy_file)
record_lengths <- tibble::tibble(
  record = tidy_dataset$record,
  length = tidy_dataset$length
)

# Comment: Add signal lengths to predictions
data <- data |>
  dplyr::left_join(record_lengths, by = "record")

cli::cli_inform(c("v" = "Added signal lengths from tidy dataset"))
# endregion Step 1

# region Step 2 - Test Mode Filtering
if (test_mode) {
  cli::cli_inform(c("i" = "Test mode: limiting to first {test_rows} rows"))
  data <- head(data, test_rows)
}
# endregion Step 2

# region Step 3 - Convert to CSV Format
cli::cli_h2("Step 2: Converting to CSV format")

csv_data <- data |>
  dplyr::mutate(
    # Comment: Calculate duration in seconds
    duration_seconds = length / const_sample_freq
  ) |>
  dplyr::select(
    record_id = record,
    window_size,
    regime_threshold,
    regime_landmark,
    min_gap_samples,
    duration_seconds,
    truth,
    pred
  )

cli::cli_inform(c("v" = "Data transformed"))
# endregion Step 3

# region Step 4 - Convert Lists to JSON
cli::cli_h2("Step 3: Converting lists to JSON format")

# Comment: Convert sample indices to time in seconds
# Comment: Format as JSON arrays for Python compatibility
csv_data <- csv_data |>
  dplyr::mutate(
    gt_times = purrr::map_chr(truth, function(x) {
      if (is.null(x) || length(x) == 0) {
        return("[]")
      }
      times <- round(x / const_sample_freq, 3)
      jsonlite::toJSON(times, auto_unbox = FALSE)
    }),
    det_times = purrr::map_chr(pred, function(x) {
      if (is.null(x) || length(x) == 0) {
        return("[]")
      }
      times <- round(x / const_sample_freq, 3)
      jsonlite::toJSON(times, auto_unbox = FALSE)
    })
  ) |>
  dplyr::select(-truth, -pred)

cli::cli_inform(c("v" = "Lists converted to JSON"))
# endregion Step 4

# region Step 5 - Add Metadata Columns
cli::cli_h2("Step 4: Adding metadata columns")

csv_data <- csv_data |>
  dplyr::mutate(
    detector = "floss",
    n_detections = purrr::map_int(det_times, ~ {
      parsed <- jsonlite::fromJSON(.x)
      if (is.null(parsed)) 0 else length(parsed)
    }),
    n_ground_truth = purrr::map_int(gt_times, ~ {
      parsed <- jsonlite::fromJSON(.x)
      if (is.null(parsed)) 0 else length(parsed)
    })
  ) |>
  dplyr::select(
    record_id,
    detector,
    window_size,
    regime_threshold,
    regime_landmark,
    min_gap_samples,
    duration_seconds,
    gt_times,
    det_times,
    n_detections,
    n_ground_truth
  )

cli::cli_inform(c("v" = "Metadata added"))
# endregion Step 5

# region Step 6 - Validate Data
cli::cli_h2("Step 5: Validating data")

# Comment: Check for NAs in critical columns
na_counts <- csv_data |>
  dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(is.na(.))))

if (any(na_counts > 0)) {
  cli::cli_alert_warning("Found NA values:")
  print(na_counts)
}

# Comment: Check for negative values
if (any(csv_data$duration_seconds < 0, na.rm = TRUE)) {
  cli::cli_alert_warning("Found negative duration_seconds")
}
if (any(csv_data$n_detections < 0, na.rm = TRUE)) {
  cli::cli_alert_warning("Found negative n_detections")
}

cli::cli_inform(c("i" = "Sample of converted data (first 3 rows):"))
print(csv_data |> dplyr::select(record_id, detector, window_size, duration_seconds) |> head(3))
# endregion Step 6

# region Step 7 - Save to CSV
cli::cli_h2("Step 6: Saving to CSV")
readr::write_csv(csv_data, output_file)
cli::cli_inform(c("v" = "Conversion complete!"))
file_size_mb <- round(file.size(output_file) / 1024 / 1024, 2)
cli::cli_inform(c("*" = "Output file: {output_file}"))
cli::cli_inform(c("*" = "Total rows: {nrow(csv_data)}"))
cli::cli_inform(c("*" = "Total columns: {ncol(csv_data)}"))
cli::cli_inform(c("*" = "File size: {file_size_mb} MB"))
# endregion Step 7

# region Step 8 - Summary
cli::cli_h2("Summary of hyperparameter combinations")
cli::cli_inform(c("i" = "Unique records: {length(unique(csv_data$record_id))}"))
cli::cli_inform(c("i" = "Window sizes: {paste(sort(unique(csv_data$window_size)), collapse = ', ')}"))
cli::cli_inform(c("i" = "Regime thresholds: {length(unique(csv_data$regime_threshold))} values"))
cli::cli_inform(c("i" = "Regime landmarks: {length(unique(csv_data$regime_landmark))} values"))
cli::cli_inform(c("i" = "Min gap samples: {paste(sort(unique(csv_data$min_gap_samples)), collapse = ', ')}"))

cli::cli_alert_success("CSV export complete!")
# endregion Step 8
# endregion Convert FLOSS Results
