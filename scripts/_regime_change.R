# Sys.setenv(TAR_PROJECT = "regime_change")

source(here("scripts", "_globals.R"))

options(target_ds_path = here("inst/extdata/afib_regimes"))

#### Pipeline: variable definitions ----
# signal sample frequency, this is a constant
const_sample_freq <- 250
const_signals <- c("time", "I", "II")
const_classes <- c("persistent_afib", "paroxysmal_afib", "non_afib")

var_resample_from <- 200
var_resample_to <- const_sample_freq

# keep only the X filenames
# var_head <- 10
# The subset that will be keep from the dataset (seq.int(240 * const_sample_freq + 1, 300 * const_sample_freq) means the last 60 seconds)
# var_subset <- seq.int(240 * const_sample_freq + 1, 300 * const_sample_freq) # last 60 secs
var_subset <- NULL
var_limit_per_class <- NULL

var_classes_include <- "paroxysmal_afib"
var_classes_exclude <- setdiff(const_classes, var_classes_include)

var_signals_include <- "II"
var_signals_exclude <- setdiff(const_signals, var_signals_include)

# window size of the filters used to remove spurious data
# var_filter_w_size <- 100 # c(100, 200)

# Matrix Profile:
## batch size of the online matrix profile algorithm (doesn't affect the results, only jumps snapshots for brevity)
var_mp_batch <- 100
## size of the online MP buffer to keep memory footprint low (multiply by the freq to have it in seconds)
var_mp_history <- 20 * const_sample_freq # 20 secs
## Threshold used on computing the MP. This concept is described elsewhere. Values above 10 use another formula
var_mp_threshold <- c(0, 0.5) # c(0, 0.5, 0.6, 0.9, 50, 60, 90)
## the window size for the MP algorithm
var_window_size <- c(200, 250)
## Multiplier for the size of the Exclusion Zone (e.g., window_size * ez == exclusion_zone)
var_ez <- 0.5
## time constraint used on MP. Don't use both mp_time_constraint and floss_time_constraint (multiply by the freq to have it in seconds)
var_mp_time_constraint <- c(0, 5 * const_sample_freq, 10 * const_sample_freq) # , 17 * const_sample_freq)

# FLOSS:
## time constraint used on FLOSS. Don't use both mp_time_constraint and floss_time_constraint (multiply by the freq to have it in seconds)
var_floss_time_constraint <- c(0, 5 * const_sample_freq, 10 * const_sample_freq) # , 17 * const_sample_freq)

# Regime change detection:
## Index position where the regime change algorithm will focus the attention
var_regime_landmark <- 3 * const_sample_freq # 3 seconds from the end
## threshold below the regime change algorithm will trigger
var_regime_threshold <- c(0.3, 0.4, 0.5) # c(0.1, 0.2, 0.3, 0.4, 0.5)

#### Targets: Define targets options ----

# use renv::install(".") to update the rcpp functions
tar_option_set(
  tidy_eval = TRUE,
  packages = c("here", "glue", "dplyr", "tidyr", "false.alarm"),
  format = "rds",
  garbage_collection = TRUE
)

# start debugme after loading all functions
if (dev_mode) {
  debugme::debugme()
}

# # All configurations used different CPUs while running the code.
library(future)
# library(future.callr)
# plan(multisession) # create top-level processes
plan(multicore) # create child processes
# plan(future.callr::callr) # create child processes with a child process

#### Pipeline: Start ----

r_input <- tar_files_input(
  #### Pipeline: Read files from directory ----
  file_paths,
  find_all_files(here::here("inst/extdata/afib_regimes"),
    data_type = "regimes",
    classes = var_classes_include
    # limit_per_class = 10
  )
)
r_dataset <- tar_target(
  #### Pipeline: Import Files to R and Select Datasets ----
  dataset,
  read_and_prepare_ecgs(file_paths,
    subset = var_subset,
    limit_per_class = var_limit_per_class,
    data_type = "regime",
    resample_from = var_resample_from,
    resample_to = var_resample_to,
    normalize = TRUE
  )
)
#### Pipeline: > NoFilters Branch ----
b_window_sizes <- tar_map(
  ##### Pipeline: > NoFilters > WindowSize Branch ----
  values = list(map_window_size = var_window_size),
  tar_target(
    ##### Pipeline: > NoFilters > WindowSize > Compute Stats ----
    ds_stats,
    process_ts_in_file(dataset,
      id = "comp_stats",
      fun = compute_companion_stats,
      params = list(
        window_size = map_window_size,
        n_workers = 1
      ),
      exclude = var_signals_exclude
    ),
    priority = 0.4,
    pattern = map(dataset)
  ),
  tar_map(
    #### Pipeline: > NoFilters > WindowSize > MP Threshold Branch ----
    values = list(map_mp_threshold = var_mp_threshold),
    tar_map(
      #### Pipeline: > NoFilters > WindowSize > MP Threshold > MP/FLOSS Constraints Branch ----
      # here the expand_grid and filter do the following:
      # -- first get all the combinations from the variables
      # -- second, keep only the combinations where one (or both) sides are zero
      # This will create a "common" branch where no constraint is applied and one branch with mp constraints and other with floss constraints.
      # Seems complicated here, but has simplified the pipeline a lot.
      values = dplyr::filter(tidyr::expand_grid(
        map_floss_time_constraint = var_floss_time_constraint,
        map_mp_time_constraint = var_mp_time_constraint
      ), !(map_floss_time_constraint > 0 & map_mp_time_constraint > 0)),
      tar_target(
        ##### Pipeline: > NoFilters > WindowSize > MP Threshold > MP/FLOSS Constraints > Compute the Matrix Profile using Stats ----
        ds_stats_mps,
        process_ts_in_file(c(dataset, ds_stats),
          id = "mps_raw_stats",
          fun = compute_s_profile_with_stats,
          params = list(
            window_size = map_window_size,
            ez = var_ez,
            progress = FALSE,
            batch = var_mp_batch,
            history = var_mp_history,
            mp_time_constraint = map_mp_time_constraint,
            threshold = map_mp_threshold
          ),
          exclude = var_signals_exclude
        ),
        priority = 0.5,
        pattern = map(dataset, ds_stats)
      ),
      tar_target(
        ##### Pipeline: > NoFilters > WindowSize > MP Threshold > MP/FLOSS Constraints > Compute FLOSS ----
        ds_stats_mps_floss,
        process_ts_in_file(ds_stats_mps,
          id = "floss_mps",
          exclude = var_signals_exclude,
          fun = compute_floss,
          params = list(
            window_size = map_window_size,
            ez = round(var_ez * map_window_size + .Machine$double.eps^0.5),
            mp_time_constraint = map_mp_time_constraint,
            floss_time_constraint = map_floss_time_constraint,
            history = var_mp_history,
            sample_freq = const_sample_freq
          )
        ),
        priority = 0.6,
        pattern = map(ds_stats_mps)
      ),
      tar_map(
        ##### Pipeline: > NoFilters > WindowSize > MP Threshold > MP/FLOSS Constraints > Regime Threshold Branch ----
        values = list(map_regime_threshold = var_regime_threshold),
        tar_target(
          ##### Pipeline: > NoFilters > WindowSize > MP Threshold > MP/FLOSS Constraints > Regime Threshold > Extract regime changes ----
          regimes,
          process_ts_in_file(ds_stats_mps_floss,
            id = "regimes",
            fun = extract_regimes,
            params = list(
              window_size = map_window_size,
              ez = var_ez,
              regime_threshold = map_regime_threshold,
              regime_landmark = var_regime_landmark, # 3 sec from the end
              # progress = FALSE,
              # batch = var_mp_batch,
              history = var_mp_history,
              mp_time_constraint = map_mp_time_constraint,
              floss_time_constraint = map_floss_time_constraint
            ),
            exclude = var_signals_exclude
          ),
          priority = 1,
          pattern = map(ds_stats_mps_floss)
        ),
        tar_target(
          ##### Pipeline: > NoFilters > WindowSize > MP Threshold > MP/FLOSS Constraints > Regime Threshold > Extract regime changes ----
          regimes_score,
          process_ts_in_file(c(dataset, regimes),
            id = "regimes_score",
            fun = compute_score_regimes,
            params = list(
              window_size = map_window_size,
              ez = var_ez,
              regime_threshold = map_regime_threshold,
              regime_landmark = var_regime_landmark, # 3 sec from the end
              gold_truth = get_file_regimes(dataset),
              # progress = FALSE,
              # batch = var_mp_batch,
              history = var_mp_history,
              mp_time_constraint = map_mp_time_constraint,
              floss_time_constraint = map_floss_time_constraint
            ),
            exclude = var_signals_exclude
          ),
          priority = 1,
          pattern = map(dataset, regimes)
        )
      )
    )
  )
)

#### Pipeline: Join targets ----
list(r_input, r_dataset, b_window_sizes)
