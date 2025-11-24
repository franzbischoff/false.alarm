# region Prepare ECG Data for Regime Detection
# This script loads ECG data, resamples, normalizes, and extracts ground truth
# Output: tidy dataset with one row per record containing truth and time series

source(here::here("scripts", "common", "read_ecg.R"), local = .GlobalEnv, encoding = "UTF-8")

suppressPackageStartupMessages({
  library(cli, quietly = TRUE, warn.conflicts = FALSE)
  library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
  library(purrr, quietly = TRUE, warn.conflicts = FALSE)
  library(tibble, quietly = TRUE, warn.conflicts = FALSE)
  library(here, quietly = TRUE, warn.conflicts = FALSE)
})

# region Configuration
# ===== DATASET SELECTION =====
# CLI override: Rscript 10_prepare_data.R <dataname>
default_dataname <- "malignantventricular"
cli_args <- commandArgs(trailingOnly = TRUE)
dataname <- if (length(cli_args) >= 1L && nzchar(cli_args[1L])) cli_args[1L] else default_dataname
cli::cli_alert_info("Dataset selected: {dataname}")

dataset_configs <- list(
  afib_regimes = list(
    const_sample_freq = 250,
    const_signals = c("time", "I", "II"),
    const_classes = c("persistent_afib", "paroxysmal_afib", "non_afib"),
    var_resample_from = 200,
    var_resample_to = 250,
    var_classes_include = "paroxysmal_afib",
    var_signals_include = "II"
  ),
  vtachyarrhythmias = list(
    const_sample_freq = 250,
    const_signals = c("time", "ECG"),
    const_classes = NULL, # No class filtering needed
    var_resample_from = 0, # No resampling
    var_resample_to = 0,
    var_classes_include = NULL,
    var_signals_include = "ECG"
  ),
  malignantventricular = list(
    const_sample_freq = 250,
    const_signals = c("time", "ECG1"),
    const_classes = NULL, # No class filtering needed
    var_resample_from = 0, # No resampling
    var_resample_to = 0,
    var_classes_include = NULL,
    var_signals_include = "ECG1"
  )
)

if (!dataname %in% names(dataset_configs)) {
  cli::cli_abort(c(
    "x" = "Unknown dataset: {dataname}",
    "i" = "Available options: {paste(names(dataset_configs), collapse = ', ')}"
  ))
}

cfg <- dataset_configs[[dataname]]
const_sample_freq <- cfg$const_sample_freq
const_signals <- cfg$const_signals
const_classes <- cfg$const_classes
var_resample_from <- cfg$var_resample_from
var_resample_to <- cfg$var_resample_to
var_classes_include <- cfg$var_classes_include
var_signals_include <- cfg$var_signals_include

# ----- Common Configuration -----
var_subset <- NULL # NULL = use entire signal
var_limit_per_class <- NULL # Set to NULL for all files, 10 for testing

# Compute derived values
if (!is.null(const_classes)) {
  var_classes_exclude <- setdiff(const_classes, var_classes_include)
} else {
  var_classes_exclude <- NULL
}
var_signals_exclude <- setdiff(const_signals, var_signals_include)

# Output configuration
output_dir <- here("output", "regime_detection", dataname, "generation")
output_file <- file.path(output_dir, "tidy_dataset.rds")
# endregion Configuration

# Skip if already processed
if (file.exists(output_file)) {
  cli::cli_alert_info("Output already exists, skipping: {output_file}")
  quit(status = 0)
}

cli::cli_h1("Regime Detection - Data Preparation")
cli::cli_inform(c("i" = "Dataset: {dataname}"))
cli::cli_inform(c("i" = "Output directory: {output_dir}"))

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  cli::cli_inform(c("v" = "Created output directory"))
}

# region Step 1 - Find Files
cli::cli_h2("Step 1: Finding ECG files")
dataset_dir <- here::here("inst", "extdata", dataname)
if (!dir.exists(dataset_dir)) {
  cli::cli_abort(c(
    "x" = "Dataset directory not found: {dataset_dir}",
    "i" = "Verify dataset name and location under inst/extdata"
  ))
}

file_paths <- find_all_files(
  dataset_dir,
  data_type = "regimes",
  classes = var_classes_include
)
cli::cli_inform(c("v" = "Found {length(file_paths)} files"))
if (length(file_paths) == 0) {
  cli::cli_abort(c(
    "x" = "No files found for dataset {dataname}",
    "i" = "Check classes/signals configuration or dataset contents"
  ))
}
# endregion Step 1

# region Step 2 - Read and Prepare Data
cli::cli_h2("Step 2: Reading and preparing ECG data")
cli::cli_inform(c("i" = "Resampling from {var_resample_from}Hz to {var_resample_to}Hz"))
cli::cli_inform(c("i" = "Normalizing: z-score"))
cli::cli_inform(c("i" = "Signal: {var_signals_include}"))

tic <- Sys.time()
dataset <- read_and_prepare_ecgs(file_paths,
  subset = var_subset,
  limit_per_class = var_limit_per_class,
  data_type = "regime",
  resample_from = var_resample_from,
  resample_to = var_resample_to,
  normalize = TRUE
)
tac <- Sys.time()
cli::cli_inform(c("v" = "Data loaded in {round(difftime(tac, tic, units = 'secs'), 2)} seconds"))
cli::cli_inform(c("i" = "Records: {length(dataset)}"))
# endregion Step 2

# region Step 3 - Transform to Tidy Format
cli::cli_h2("Step 3: Transforming to tidy format")
tidy_dataset <- purrr::map_dfr(dataset, function(x) {
  regimes <- attr(x, "regimes")
  if (length(regimes) == 0) {
    return(NULL) # Skip files with no regime changes
  }
  if (length(regimes) == 1 && regimes == 0) {
    return(NULL) # Skip files with only zero
  }
  tibble::tibble(
    truth = list(regimes),
    ts = list(x[[var_signals_include]])
  )
}, .id = "record")

cli::cli_inform(c("i" = "Records with regime changes: {nrow(tidy_dataset)}"))
# endregion Step 3

# region Step 4 - Clean Ground Truth
cli::cli_h2("Step 4: Cleaning ground truth positions")
for (i in seq_along(tidy_dataset$truth)) {
  tidy_dataset$truth[[i]] <- clean_truth(
    tidy_dataset$truth[[i]],
    length(tidy_dataset$ts[[i]])
  )
}

# Add signal length for later use
tidy_dataset <- tidy_dataset |>
  dplyr::mutate(length = purrr::map_int(ts, length))

cli::cli_inform(c("i" = "Total samples across all records: {sum(tidy_dataset$length)}"))
cli::cli_inform(c("i" = "Average signal length: {round(mean(tidy_dataset$length), 0)} samples"))
cli::cli_inform(c("i" = "Average duration: {round(mean(tidy_dataset$length) / const_sample_freq, 1)} seconds"))
# endregion Step 4

# region Step 5 - Save Tidy Dataset
cli::cli_h2("Step 5: Saving tidy dataset")
saveRDS(tidy_dataset, file = output_file, compress = "xz")
cli::cli_inform(c("v" = "Saved to: {output_file}"))
file_size_mb <- round(file.size(output_file) / 1024 / 1024, 2)
cli::cli_inform(c("*" = "File size: {file_size_mb} MB"))
cli::cli_inform(c("*" = "Columns: {paste(names(tidy_dataset), collapse = ', ')}"))
cli::cli_inform(c("*" = "Rows: {nrow(tidy_dataset)}"))
# endregion Step 5

cli::cli_alert_success("Data preparation complete!")
# endregion Prepare ECG Data
