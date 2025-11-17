# region Do computation
# Comment: Load all helper functions from the common directory
# Comment: This includes data reading, filtering, and preprocessing utilities
script_files <- list.files(here::here("scripts", "common"), pattern = "*.R")
purrr::walk(here::here("scripts", "common", script_files), source, local = .GlobalEnv, encoding = "UTF-8")
rm(script_files)

# Comment: Load core functions for FLOSS algorithm (Matrix Profile-based regime detection)
source(here::here("scripts", "helpers", "glue_fmt.R"), local = .GlobalEnv, encoding = "UTF-8")
source(here::here("R", "floss_train.R"), local = .GlobalEnv, encoding = "UTF-8") # nolint
source(here::here("R", "floss_predict.R"), local = .GlobalEnv, encoding = "UTF-8") # nolint
source(here::here("scripts", "common", "score_floss.R"), local = .GlobalEnv, encoding = "UTF-8") # nolint

#########
# dataname <- "vtachyarrhythmias"
# # signal sample frequency, this is a constant
# const_sample_freq <- 250
# const_signals <- c("time", "ECG")

# var_resample_from <- 0
# var_resample_to <- 0

# var_subset <- NULL # 1:10000 # NULL
# var_limit_per_class <- NULL

# var_classes_include <- NULL
# var_classes_exclude <- NULL

# var_signals_include <- "ECG"
# var_signals_exclude <- setdiff(const_signals, var_signals_include)
# #########

# dataname <- "malignantventricular"
# # signal sample frequency, this is a constant
# const_sample_freq <- 250
# const_signals <- c("time", "ECG1")

# var_resample_from <- 0
# var_resample_to <- 0

# var_subset <- NULL # 1:10000 # NULL
# var_limit_per_class <- NULL

# var_classes_include <- NULL
# var_classes_exclude <- NULL

# var_signals_include <- "ECG1"
# var_signals_exclude <- setdiff(const_signals, var_signals_include)
#########
# Comment: Dataset configuration - AFib Regimes (Paroxysmal Atrial Fibrillation)
# Comment: This dataset contains ECG signals with regime changes in AFib episodes
dataname <- "afib_regimes"

# Comment: ECG signal parameters
const_sample_freq <- 250 # Hz - standard ECG sampling frequency
const_signals <- c("time", "I", "II") # Available ECG leads
const_classes <- c("persistent_afib", "paroxysmal_afib", "non_afib")

# Comment: Resampling configuration (from 200Hz to 250Hz for standardization)
var_resample_from <- 200
var_resample_to <- const_sample_freq

# Comment: Data filtering options
var_subset <- NULL # 1:10000  # Comment: NULL = use entire signal
var_limit_per_class <- NULL # Comment: NULL = no limit on files per class

# Comment: Focus only on paroxysmal AFib (episodes that come and go)
var_classes_include <- "paroxysmal_afib"
var_classes_exclude <- setdiff(const_classes, var_classes_include)

# Comment: Use Lead II for analysis (most common for rhythm analysis)
var_signals_include <- "II"
var_signals_exclude <- setdiff(const_signals, var_signals_include)
########

# Comment: Step 1 - Find all ECG files matching the criteria
# Comment: Searches for regime-labeled files in the specified dataset directory
file_paths <- find_all_files(here::here("inst", "extdata", dataname),
  data_type = "regimes",
  classes = var_classes_include
  # limit_per_class = 10  # Comment: Uncomment to limit files for testing
)

# Comment: Step 2 - Read and prepare the data
# Comment: Loads ECG signals, resamples to 250Hz, normalizes (z-score), and extracts regime labels
dataset <- read_and_prepare_ecgs(file_paths,
  subset = var_subset,
  limit_per_class = var_limit_per_class,
  data_type = "regime",
  resample_from = var_resample_from,
  resample_to = var_resample_to,
  normalize = TRUE # Comment: Z-score normalization for better MP computation
)

# Comment: Step 3 - Transform dataset into tidy format (one row per file)
# Comment: Extract ground truth (regime change points) and time series data
tidy_dataset <- purrr::map_dfr(dataset, function(x) {
  regimes <- attr(x, "regimes") # Comment: Ground truth regime change positions
  if (length(regimes) == 0) {
    return(NULL) # Comment: Skip files with no regime changes in the subset
  }
  if (length(regimes) == 1 && regimes == 0) {
    return(NULL) # Comment: Skip files with only zero (no valid changes)
  }
  tibble::tibble(truth = list(regimes), ts = list(x[[var_signals_include]]))
}, .id = "id")

# Comment: Clean and validate ground truth positions
# Comment: Ensures regime positions are within signal bounds and properly formatted
for (i in seq_along(tidy_dataset$truth)) {
  tidy_dataset$truth[[i]] <- clean_truth(tidy_dataset$truth[[i]], length(tidy_dataset$ts[[i]]))
}

rm(dataset) # Comment: Free memory - no longer needed

#############
# Comment: Grid search hyperparameter - Matrix Profile window size
# Comment: Testing 3 values: 350, 375, 400 (approximately 1.4-1.6 seconds at 250Hz)
var_window_size <- seq(350, 400, by = 25)
split <- 5 # Comment: Split identifier for partial results saving
#############

tic <- Sys.time()
# Comment: Step 4 - Compute Matrix Profile using FLOSS algorithm
# Comment: Set up parallel processing for faster computation
future::plan(future::multicore) # Comment: Use all available CPU cores

# Comment: FLOSS (Fast Low-cost Online Semantic Segmentation) computation
# Comment: The Matrix Profile is computed in batches of 100 samples with a history buffer of 5000 samples
# Comment: This is memory-intensive - processing one window size at a time
floss_dataset <- NULL
for (w in var_window_size) {
  cli::cli_inform(c("i" = "Starting processing window size {w}"))
  tictic <- Sys.time()
  # Comment: Parallel computation of FLOSS for all time series
  # Comment: floss_train_regimes returns the arc curve needed for regime detection
  floss <- furrr::future_map(tidy_dataset$ts, ~ {
    floss_train_regimes(.x, w, 0, 0)
  }, .options = furrr::furrr_options(seed = NULL))
  checkmate::qassert(floss, "L+") # Comment: Validate output is a list
  floss_dataset <- dplyr::bind_rows(
    floss_dataset,
    tibble::add_column(tidy_dataset, window_size = w, floss = floss) |>
      dplyr::select(-ts) # Comment: Remove raw time series to save memory
  )
  tactac <- Sys.time()
  cli::cli_inform(c("v" = "Finished processing window size {w} in {round(difftime(tactac, tictic, units = 'mins'), 2)} minutes"))
}
tac <- Sys.time()
cli::cli_inform(c("!" = "Time taken to compute floss: {round(difftime(tac, tic, units = 'mins'), 2)} minutes"))

rm(tidy_dataset) # Comment: Free memory - time series no longer needed

# Comment: Grid search hyperparameters for regime detection
# Comment: regime_threshold: sensitivity for detecting regime changes (18 values)
# Comment: regime_landmark: temporal lag (in seconds) where threshold is applied to arc curve (15 values)
# Comment:   - A landmark of 2 means the detection occurs 2 seconds behind the current streaming position
var_regime_threshold <- seq(0.05, 0.9, by = 0.05)
var_regime_landmark <- seq(2, 9, by = 0.5)

# Comment: Create complete grid: 18 thresholds × 15 landmarks = 270 combinations
# Comment: Combined with 3 window sizes = 810 models per record
grid <- expand.grid(
  regime_threshold = var_regime_threshold,
  regime_landmark = var_regime_landmark
)

tic <- Sys.time()
# Comment: Step 5 - Generate predictions for all hyperparameter combinations
# Comment: For each FLOSS result, test all 270 (threshold × landmark) combinations
floss_preds <- purrr::map_dfr(seq_len(nrow(floss_dataset)), function(i) {
  cli::cli_inform(c("i" = "Starting processing row {i} of {nrow(floss_dataset)}"))
  current_row <- floss_dataset[i, ]

  tictic <- Sys.time()
  # Comment: Apply all grid combinations to this FLOSS result
  pred_dataset <- purrr::map_dfr(seq_len(nrow(grid)), function(j) {
    rt <- grid$regime_threshold[j]
    rl <- grid$regime_landmark[j]

    # Comment: floss_predict uses the arc curve to identify regime change points
    # Comment: clean_pred removes predictions too close to each other (within 200 samples), prioritize the first
    pred <- floss_predict(
      current_row$floss[[1]],
      current_row$window_size,
      0,
      rt,
      rl
    ) |> clean_pred(200, FALSE) # FALSE keeps first, TRUE keeps last

    if (is.null(pred)) {
      cli::cli_inform(c("!" = "No prediction for row {i} and grid {j}"))
    }

    # Comment: Store predictions with corresponding hyperparameters
    current_row |>
      dplyr::select(-floss) |> # Comment: Remove FLOSS object to save memory
      dplyr::mutate(
        regime_threshold = rt,
        regime_landmark = rl,
        pred = list(pred) # Comment: Predicted regime change positions
      )
  })

  tactac <- Sys.time()
  cli::cli_inform(c("v" = "Finished processing row {i} in {round(difftime(tactac, tictic, units = 'mins'), 2)} minutes"))
  pred_dataset
})
tac <- Sys.time()
cli::cli_inform(c("!" = "Time taken to compute predictions: {round(difftime(tac, tic, units = 'mins'), 2)} minutes"))

tic <- Sys.time()
# Comment: Step 6 - Compute evaluation scores
# Comment: score_pr is a custom F-score with temporal penalty
# Comment: Parameters: tolerance of 10s and 4s for matching predicted vs true regime changes
floss_scores <- list()
for (i in seq_len(nrow(floss_preds))) {
  score <- score_pr(floss_preds$truth[[i]], floss_preds$pred[[i]], const_sample_freq, 10, 4)
  floss_scores[[i]] <- 1 - score # Comment: Invert score (higher is better)
}

# Comment: Add scores to predictions and format hyperparameters
final_dataset <- tibble::add_column(floss_preds, score = unlist(floss_scores))
final_dataset$regime_threshold <- round(final_dataset$regime_threshold, 2)
final_dataset$regime_landmark <- round(final_dataset$regime_landmark, 1)
final_dataset$window_size <- as.integer(final_dataset$window_size)
tac <- Sys.time()
cli::cli_inform(c("!" = "Time taken to compute scores: {round(difftime(tac, tic, units = 'mins'), 2)} minutes"))

# Comment: Rename 'id' to 'record' for clarity and save partial results
final_dataset <- final_dataset |> dplyr::rename(record = id)
saveRDS(final_dataset, file = here::here("output", glue::glue("{dataname}-{split}.rds")), compress = "xz")
# endregion Do computation

# region Add Baseline
# Comment: Step 7 - Add baseline comparisons
# Comment: Baseline models predict regime changes at fixed intervals (naive approach)
# Comment: Reload the complete dataset (this assumes all splits were merged)
final_dataset <- readRDS(here::here("output", glue::glue("{dataname}.rds")))

# Comment: Rebuild tidy_dataset to get signal lengths (needed for baseline generation)
tidy_dataset <- dplyr::mutate(tidy_dataset, length = purrr::map_int(ts, length))

# Comment: Baseline 1 - Predict regime change every 1 second (250 samples at 250Hz)
# Comment: This represents a very aggressive detection strategy
baseline_timing <- 250 # every second
tidy_dataset <- tidy_dataset |>
  dplyr::mutate(baseline = purrr::map(tidy_dataset$length, ~ seq(baseline_timing, .x, by = baseline_timing)))

# Comment: Baseline 2 - Predict regime change every 4 seconds (1000 samples at 250Hz)
# Comment: This is a more conservative detection strategy
baseline_timing_4 <- 250 * 4 # every 4 second
tidy_dataset <- tidy_dataset |>
  dplyr::mutate(baseline4 = purrr::map(tidy_dataset$length, ~ seq(baseline_timing_4, .x, by = baseline_timing_4)))

# Comment: Join baseline predictions with the main dataset
# Comment: Each model's predictions will be compared against both baselines
final_dataset <- final_dataset |>
  dplyr::left_join(tidy_dataset |> dplyr::select(id, length, baseline, baseline4), by = c("record" = "id"))

# Comment: Compute baseline scores using the same metric as FLOSS models
# Comment: Baseline 1 score - fixed 1-second interval predictions
baseline_scores <- list()
for (i in seq_len(nrow(final_dataset))) {
  score <- score_pr(final_dataset$truth[[i]], final_dataset$baseline[[i]], const_sample_freq, 10, 4)
  baseline_scores[[i]] <- 1 - score # Comment: Invert for consistency (higher is better)
}

# Comment: Baseline 2 score - fixed 4-second interval predictions
baseline_scores4 <- list()
for (i in seq_len(nrow(final_dataset))) {
  score <- score_pr(final_dataset$truth[[i]], final_dataset$baseline4[[i]], const_sample_freq, 10, 4)
  baseline_scores4[[i]] <- 1 - score # Comment: Invert for consistency (higher is better)
}

# Comment: Add baseline scores to the dataset for comparison
final_dataset <- tibble::add_column(final_dataset, baseline_score = unlist(baseline_scores), .after = "baseline")
final_dataset <- tibble::add_column(final_dataset, baseline4_score = unlist(baseline_scores4), .after = "baseline4")

# Comment: Save final dataset with FLOSS predictions and baseline comparisons
# Comment: This file will be used for cross-validation and comparison analysis
saveRDS(final_dataset, file = here::here("output", glue::glue("{dataname}.rds")), compress = "xz")
