# region Convert FLOSS RDS to CSV format
# Comment: This script converts the FLOSS results from RDS format to CSV format
# Comment: compatible with the Python evaluation pipeline

# Comment: Load required libraries
library(dplyr)
library(purrr)
library(readr)
library(jsonlite)
library(here)
library(glue)
library(cli)

# Comment: Configuration
dataname <- "afib_regimes"
output_filename <- glue("{dataname}_predictions_intermediate.csv")
test_mode <- FALSE # Comment: Set to FALSE to export all data
test_rows <- 1000 # Comment: Number of rows for test export

# Comment: Step 1 - Load FLOSS results
cli::cli_inform(c("i" = "Loading FLOSS results from {dataname}.rds"))
data <- readRDS(here("output", glue("{dataname}.rds")))
cli::cli_inform(c("v" = "Loaded {nrow(data)} rows with {ncol(data)} columns"))

# Comment: Step 2 - Limit to test rows if in test mode
if (test_mode) {
  cli::cli_inform(c("i" = "Test mode: limiting to first {test_rows} rows"))
  data <- head(data, test_rows)
}

# Comment: Step 3 - Convert to CSV format
cli::cli_inform(c("i" = "Converting to CSV format"))

csv_data <- data %>%
  mutate(
    # Comment: Identification columns
    record_id = record,
    detector = "floss",

    # Comment: min_gap_samples is currently fixed at 200 in clean_pred()
    # TODO: This should become a grid search parameter in the future
    min_gap_samples = 200,

    # Comment: Convert duration from samples to seconds (250 Hz)
    duration_seconds = length / 250.0,

    # Comment: Convert ground truth indices to times (samples → seconds)
    gt_times = map(truth, function(x) if (is.null(x) || length(x) == 0) numeric(0) else x / 250.0),

    # Comment: Convert prediction indices to times (samples → seconds)
    det_times = map(pred, function(x) if (is.null(x) || length(x) == 0) numeric(0) else x / 250.0),

    # Comment: Count number of detections and ground truth events using lengths()
    n_detections = lengths(pred),
    n_ground_truth = lengths(truth)
  ) %>%
  # Comment: Select only necessary columns in the correct order
  select(
    # Identification
    record_id, detector,
    # FLOSS hyperparameters
    window_size, regime_threshold, regime_landmark, min_gap_samples,
    # Duration
    duration_seconds,
    # Ground truth and detections (as times in seconds)
    gt_times, det_times,
    # Counts
    n_detections, n_ground_truth
  ) # Comment: Step 4 - Convert list columns to JSON format (Python-compatible)
cli::cli_inform(c("i" = "Converting lists to JSON format"))

csv_data <- csv_data %>%
  mutate(
    # Comment: Convert R vectors to Python list strings [1.0, 2.0, 3.0]
    gt_times = map_chr(gt_times, ~ toJSON(.x, auto_unbox = FALSE)),
    det_times = map_chr(det_times, ~ toJSON(.x, auto_unbox = FALSE))
  )

# Comment: Step 5 - Validate data
cli::cli_inform(c("i" = "Validating data"))

# Comment: Basic validation - check for NAs and consistency
if (any(is.na(csv_data$record_id))) {
  cli::cli_warn(c("!" = "Found NA values in record_id"))
}

if (any(csv_data$n_detections < 0) || any(csv_data$n_ground_truth < 0)) {
  cli::cli_warn(c("!" = "Found negative counts in n_detections or n_ground_truth"))
}

# Comment: Display sample of the data
cli::cli_inform(c("i" = "Sample of converted data (first 3 rows):"))
print(head(csv_data[, c("record_id", "detector", "window_size", "duration_seconds")], 3))

# Comment: Step 6 - Save to CSV
output_path <- here("output", output_filename)

if (test_mode) {
  output_path <- here("output", glue("test_{output_filename}"))
}

cli::cli_inform(c("i" = "Saving to {output_path}"))
write_csv(csv_data, output_path)

# Comment: Display summary statistics
cli::cli_inform(c("v" = "Conversion complete!"))
cli::cli_inform(c("*" = "Output file: {output_path}"))
cli::cli_inform(c("*" = "Total rows: {nrow(csv_data)}"))
cli::cli_inform(c("*" = "Total columns: {ncol(csv_data)}"))
file_size_mb <- round(file.size(output_path) / 1024 / 1024, 2)
cli::cli_inform(c("*" = "File size: {file_size_mb} MB"))

# Comment: Display unique values summary
cli::cli_inform(c("i" = "Summary of hyperparameter combinations:"))
cli::cli_inform(c("*" = "Unique records: {length(unique(csv_data$record_id))}"))
cli::cli_inform(c("*" = "Window sizes: {paste(unique(csv_data$window_size), collapse=', ')}"))
cli::cli_inform(c("*" = "Regime thresholds: {length(unique(csv_data$regime_threshold))} values"))
cli::cli_inform(c("*" = "Regime landmarks: {length(unique(csv_data$regime_landmark))} values"))
cli::cli_inform(c("*" = "min_gap_samples: {unique(csv_data$min_gap_samples)}"))

# endregion Convert FLOSS RDS to CSV format
