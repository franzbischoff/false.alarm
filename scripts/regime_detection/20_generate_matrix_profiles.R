# region Generate Matrix Profiles
# Comment: This script computes Matrix Profiles for all window sizes (25-400)
# Comment: Memory-optimized: processes one window size at a time
# Comment: Input: tidy_dataset.rds (from step 10)
# Comment: Output: matrix_profiles_w{size}.rds for each window size

library(cli)
library(dplyr)
library(purrr)
library(tibble)
library(here)
library(furrr)
library(future)

# Comment: Load FLOSS functions
source(here::here("scripts", "helpers", "glue_fmt.R"), local = .GlobalEnv, encoding = "UTF-8")
source(here::here("R", "floss_train.R"), local = .GlobalEnv, encoding = "UTF-8")

# region Configuration
# Comment: ===== DATASET SELECTION =====
# Comment: Must match dataset from script 10

dataname <- "afib_regimes"
# dataname <- "vtachyarrhythmias"
# dataname <- "malignantventricular"

const_sample_freq <- 250

# Comment: Window sizes to test (25 to 400 in steps of 25)
# Comment: This represents window lengths from 0.1 to 1.6 seconds at 250Hz
var_window_size <- seq(25, 400, by = 25)

# Comment: Parallel processing configuration
n_workers <- 20 # Adjust based on available CPU/memory

# Comment: Input/Output paths
input_dir <- here("output", "regime_detection", dataname, "generation")
output_dir <- input_dir
input_file <- file.path(input_dir, "tidy_dataset.rds")
# endregion Configuration

cli::cli_h1("Regime Detection - Matrix Profile Generation")
cli::cli_inform(c("i" = "Dataset: {dataname}"))
cli::cli_inform(c("i" = "Window sizes: {length(var_window_size)} ({min(var_window_size)} to {max(var_window_size)})"))
cli::cli_inform(c("i" = "Parallel workers: {n_workers}"))

# region Step 1 - Load Tidy Dataset
cli::cli_h2("Step 1: Loading tidy dataset")
if (!file.exists(input_file)) {
  cli::cli_abort(c(
    "x" = "Input file not found: {input_file}",
    "i" = "Run 10_prepare_data.R first"
  ))
}

tidy_dataset <- readRDS(input_file)
cli::cli_inform(c("v" = "Loaded {nrow(tidy_dataset)} records"))
cli::cli_inform(c("i" = "Total samples: {sum(tidy_dataset$length)}"))
# endregion Step 1

# region Step 2 - Compute Matrix Profiles
cli::cli_h2("Step 2: Computing Matrix Profiles")
cli::cli_inform(c("i" = "Processing one window size at a time to minimize memory usage"))

# Comment: Set up parallel processing
future::plan(future::multicore, workers = n_workers)

total_tic <- Sys.time()

for (w in var_window_size) {
  output_file <- file.path(output_dir, glue::glue("matrix_profiles_w{w}.rds"))

  # Comment: Skip if already computed
  if (file.exists(output_file)) {
    cli::cli_inform(c("!" = "Window {w}: Already exists, skipping"))
    next
  }

  cli::cli_alert_info("Window {w}: Starting computation ({nrow(tidy_dataset)} records in parallel)")
  tic <- Sys.time()

  # Comment: Compute FLOSS for all time series in parallel
  # Comment: floss_train_regimes returns the arc curve (Matrix Profile derivative)
  # Comment: Batch size: 100 samples, History buffer: 5000 samples
  floss_results <- furrr::future_map(
    tidy_dataset$ts,
    ~ floss_train_regimes(.x, w, 0, 0),
    .options = furrr::furrr_options(seed = NULL)
  ) # Comment: Validate output
  checkmate::qassert(floss_results, "L+")

  # Comment: Create dataset with FLOSS results
  mp_dataset <- tibble::tibble(
    record = tidy_dataset$record,
    window_size = w,
    floss = floss_results,
    length = tidy_dataset$length
  )

  # Comment: Save to disk
  saveRDS(mp_dataset, file = output_file, compress = "xz")

  tac <- Sys.time()
  elapsed_mins <- round(difftime(tac, tic, units = "mins"), 2)
  file_size_mb <- round(file.size(output_file) / 1024 / 1024, 2)

  cli::cli_alert_success("Window {w}: Complete in {elapsed_mins} mins ({file_size_mb} MB)")

  # Comment: Free memory
  rm(floss_results, mp_dataset)
  gc()
}

total_tac <- Sys.time()
total_elapsed <- round(difftime(total_tac, total_tic, units = "mins"), 2)
# endregion Step 2

# region Step 3 - Summary
cli::cli_h2("Summary")
cli::cli_inform(c("v" = "Total time: {total_elapsed} minutes"))
cli::cli_inform(c("i" = "Output directory: {output_dir}"))
cli::cli_inform(c("i" = "Files created:"))

for (w in var_window_size) {
  output_file <- file.path(output_dir, glue::glue("matrix_profiles_w{w}.rds"))
  if (file.exists(output_file)) {
    file_size_mb <- round(file.size(output_file) / 1024 / 1024, 2)
    cli::cli_inform(c("*" = "  matrix_profiles_w{w}.rds ({file_size_mb} MB)"))
  }
}

cli::cli_alert_success("Matrix Profile generation complete!")
# endregion Step 3
# endregion Generate Matrix Profiles
