# region Prepare ECG Data for Regime Detection
# Comment: This script loads ECG data, resamples, normalizes, and extracts ground truth
# Comment: Output: tidy dataset with one row per record containing truth and time series

# Comment: Load all helper functions
script_files <- list.files(here::here("scripts", "common"), pattern = "*.R")
purrr::walk(here::here("scripts", "common", script_files), source, local = .GlobalEnv, encoding = "UTF-8")
rm(script_files)

library(cli)
library(dplyr)
library(purrr)
library(tibble)
library(here)

# region Configuration
# Comment: ===== DATASET SELECTION =====
# Comment: Uncomment ONE dataset configuration below

# Comment: ----- AFib Regimes (Paroxysmal Atrial Fibrillation) -----
dataname <- "afib_regimes"
const_sample_freq <- 250
const_signals <- c("time", "I", "II")
const_classes <- c("persistent_afib", "paroxysmal_afib", "non_afib")
var_resample_from <- 200
var_resample_to <- const_sample_freq
var_classes_include <- "paroxysmal_afib"
var_signals_include <- "II"

# Comment: ----- VTach Arrhythmias -----
# dataname <- "vtachyarrhythmias"
# const_sample_freq <- 250
# const_signals <- c("time", "ECG")
# const_classes <- NULL  # No class filtering needed
# var_resample_from <- 0  # No resampling
# var_resample_to <- 0
# var_classes_include <- NULL
# var_signals_include <- "ECG"

# Comment: ----- Malignant Ventricular -----
# dataname <- "malignantventricular"
# const_sample_freq <- 250
# const_signals <- c("time", "ECG1")
# const_classes <- NULL  # No class filtering needed
# var_resample_from <- 0  # No resampling
# var_resample_to <- 0
# var_classes_include <- NULL
# var_signals_include <- "ECG1"

# Comment: ----- Common Configuration -----
var_subset <- NULL # NULL = use entire signal
var_limit_per_class <- NULL # Set to NULL for all files, 10 for testing

# Comment: Compute derived values
if (!is.null(const_classes)) {
  var_classes_exclude <- setdiff(const_classes, var_classes_include)
} else {
  var_classes_exclude <- NULL
}
var_signals_exclude <- setdiff(const_signals, var_signals_include)

# Comment: Output configuration
output_dir <- here("output", "regime_detection", dataname, "generation")
output_file <- file.path(output_dir, "tidy_dataset.rds")
# endregion Configuration

cli::cli_h1("Regime Detection - Data Preparation")
cli::cli_inform(c("i" = "Dataset: {dataname}"))
cli::cli_inform(c("i" = "Output directory: {output_dir}"))

# Comment: Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  cli::cli_inform(c("v" = "Created output directory"))
}

# region Step 1 - Find Files
cli::cli_h2("Step 1: Finding ECG files")
file_paths <- find_all_files(
  here::here("inst", "extdata", dataname),
  data_type = "regimes",
  classes = var_classes_include
)
cli::cli_inform(c("v" = "Found {length(file_paths)} files"))
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

# Comment: Add signal length for later use
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
