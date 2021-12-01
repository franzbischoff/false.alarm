library(targets)
library(tarchetypes)
library(purrr)

#### Dev variables ----

dev_mode <- FALSE # !identical(Sys.getenv("DEBUGME"), "")
skip_graphics <- TRUE

if (dev_mode) {
  # I know I shall not use this
  # devtools::load_all(".")
  # Sys.setenv(DEBUGME_OUTPUT_FILE = "debugme.log")
}

# examples: a109l_aVF_300_0_raw
#           a161l (asystole) 300_1250, false neg

# TODO: how near to consider to update the MP

# TODO: density of similarity changes a little bit from the size of constraint, but a lot due to the window size
# TODO: but, the sum of the density from 50-100 doesnt change in any case


#### Physionet's dataset definitions ----
# Asystole: No QRS for at least 4 seconds
# Ventricular Flutter/Fibrillation: Fibrillatory, flutter, or oscillatory waveform for at least 4 .leap.seconds

# Extreme Bradycardia: Heart rate lower than 40 bpm for 5 consecutive beats
# Extreme Tachycardia: Heart rate higher than 140 bpm for 17 consecutive beats
# Ventricular Tachycardia: 5 or more ventricular beats with heart rate higher than 100 bpm

#### General: Config variables ----

options(tidyverse.quiet = TRUE)
options(target_ds_path = "inst/extdata/physionet")
options(crayon.enabled = TRUE)
cluster <- FALSE
backend <- "FUTURE"

#### Targets: Load Scripts ----

# Load all scripts
script_files <- list.files(here::here("scripts"), pattern = "*.R")
sapply(here::here("scripts", script_files), source, local = .GlobalEnv, encoding = "UTF-8")
rm(script_files)

#### Targets: Setup engine ----

if (isFALSE(cluster)) { ## Locally
  if (backend == "FUTURE") {
    library(future)
    library(future.callr)
    future::plan(callr)
  } else {
    options(
      clustermq.scheduler = "multiprocess",
      clustermq.ssh.host = NULL,
      clustermq.ssh.log = NULL
    )
    library(clustermq)
  }
} else {
  if (backend == "FUTURE") { ## cluster # tar_make_future(workers = 4)
    # *** If using future for multithreading / clustering ***
    library(future)
    library(future.batchtools)

    future::plan(
      strategy = future.batchtools::batchtools_custom,
      cluster.functions = batchtools::makeClusterFunctionsSSH(
        list(
          # batchtools::Worker$new("franz@192.168.1.237", ncpus = 4)
          batchtools::Worker$new("localhost", ncpus = 4)
        ),
        # fs.latency = 1000
      )
    )
  } else { # tar_make_clustermq(workers = 3)
    options(
      clustermq.scheduler = "ssh",
      clustermq.ssh.host = "franz@192.168.1.237", # use your user and host, obviously
      clustermq.ssh.log = "~/cmq_ssh.log" # log for easier debugging # nolint
    )
    library(clustermq)
  }
}

#### Targets: Define targets options ----

# use renv::install(".") to update the rcpp functions
tar_option_set(
  packages = c("dplyr", "false.alarm"),
  format = "rds",
  # resources = tar_resources(
  #   #   #   #   # *** If using clustermq for multithreading / clustering ***
  #   clustermq = tar_resources_clustermq(
  #     template = list(num_cores = 4)
  #   ), # or n_jobs??
  #   #   #   #   # *** If using future for multithreading / clustering ***
  #   future = tar_resources_future(
  #     resources = list(num_cores = 4)
  #   )
  # ),
  garbage_collection = TRUE,
  # workspace_on_error = TRUE,
  memory = "transient",
  # storage = "main",
  # envir = globalenv(),
  # iteration = "list",
  # debug = "ds_stats_mps_floss2_0_1250_0_200_fa978ffc",
  # cue = tar_cue(
  #   mode = "thorough",
  #   command = TRUE,
  #   depend = TRUE,
  #   format = TRUE,
  #   iteration = TRUE,
  #   file = FALSE
  # ),
  imports = "false.alarm" # TODO: remove when there is no change on package functions. Clears the graph.
)

#### Pipeline: variable definitions ----
# signal sample frequency, this is a constant
const_sample_freq <- 250
# keep only the X filenames
var_head <- 10
# The subset that will be keep from the dataset (seq.int(240 * const_sample_freq + 1, 300 * const_sample_freq) means the last 60 seconds)
var_subset <- seq.int(240 * const_sample_freq + 1, 300 * const_sample_freq) # last 60 secs
# keep only the X files per type (asystole, bradycardia, etc)
var_limit_per_type <- 2
# tracks to exclude from the files
var_exclude <- c("time", "V", "ABP", "PLETH", "RESP")

# window size of the filters used to remove spurious data
var_filter_w_size <- 100 # c(100, 200)

# Matrix Profile:
## batch size of the online matrix profile algorithm (doesn't affect the results, only jumps snapshots for brevity)
var_mp_batch <- 100
## size of the online MP buffer to keep memory footprint low (multiply by the freq to have it in seconds)
var_mp_history <- 20 * const_sample_freq # 20 secs
## Threshold used on computing the MP. This concept is described elsewhere. Values above 10 use another formula
var_mp_threshold <- 0 # c(0, 0.5, 0.6, 0.9, 50, 60, 90)
## the window size for the MP algorithm
var_window_size <- 200 # c(200, 250)
## Multiplier for the size of the Exclusion Zone (e.g., window_size * ez == exclusion_zone)
var_ez <- 0.5
## time constraint used on MP. Don't use both mp_time_constraint and floss_time_constraint (multiply by the freq to have it in seconds)
var_mp_time_constraint <- c(5 * const_sample_freq, 10 * const_sample_freq) # , 17 * const_sample_freq)

# FLOSS:
## time constraint used on FLOSS. Don't use both mp_time_constraint and floss_time_constraint (multiply by the freq to have it in seconds)
var_floss_time_constraint <- c(0, 5 * const_sample_freq, 10 * const_sample_freq) # , 17 * const_sample_freq)

# Regime change detection:
## Index position where the regime change algorithm will focus the attention
var_regime_landmark <- 3 * const_sample_freq # 3 seconds from the end
## threshold below the regime change algorithm will trigger
var_regime_threshold <- c(0.3, 0.4, 0.5) # c(0.1, 0.2, 0.3, 0.4, 0.5)

# debug(process_ts_in_file)
# tar_make(names = ds_mp_filtered, callr_function = NULL)

# start debugme after loading all functions
if (dev_mode) {
  debugme::debugme()
}
#### Pipeline: Start ----

l_input <- tar_files_input(
  #### Pipeline: Read files from directory ----
  file_paths,
  # tail(head(find_all_files(types = "all"), var_head), 10),
  find_all_files(types = "asystole") # , "bradycardia", "tachycardia", "vfib", "vtachy"
)
l_dataset <- tar_target(
  #### Pipeline: Import Files to R and Select Datasets ----
  dataset,
  read_and_prepare_ecgs(file_paths,
    subset = var_subset,
    true_alarm = TRUE,
    limit_per_type = var_limit_per_type
  )
)
# tar_target(
#   neg_training_floss,
#   create_floss_training()
# ),
# test <- tar_target(
#   ds_stats,
#   process_ts_in_file(dataset,
#     id = "comp_stats",
#     fun = compute_companion_stats,
#     params = list(
#       window_size = map_window_size,
#       n_workers = 1
#     ),
#     exclude = var_exclude
#   ),
#   pattern = map(dataset)
# )
#### Pipeline: > NoFilters Branch ----
l_w_size_root <- tar_map(
  ##### Pipeline: > NoFilters > WindoSize Branch ----
  list(map_window_size = var_window_size),
  tar_target(
    ##### Pipeline: > NoFilters > WindoSize > Compute Stats ----
    ds_stats,
    process_ts_in_file(dataset,
      id = "comp_stats",
      fun = compute_companion_stats,
      params = list(
        window_size = map_window_size,
        n_workers = 1
      ),
      exclude = var_exclude
    ),
    pattern = map(dataset)
  ),
  l_mp_thr_root <- tar_map(
    #### Pipeline: > NoFilters > WindoSize > MP Threshold Branch ----
    list(map_mp_threshold = var_mp_threshold),
    tar_target(
      ##### Pipeline: > NoFilters > WindoSize > MP Threshold > Compute the Matrix Profile with no constraints ----
      ds_stats_mps_nc,
      process_ts_in_file(c(dataset, ds_stats),
        id = "mps_raw_stats_no_constraint",
        fun = compute_s_profile_with_stats,
        params = list(
          window_size = map_window_size,
          ez = var_ez,
          progress = FALSE,
          batch = var_mp_batch,
          history = var_mp_history,
          mp_time_constraint = 0,
          threshold = map_mp_threshold
        ),
        exclude = var_exclude
      ),
      pattern = map(dataset, ds_stats)
    ),
    l_mp_tconstr_root <- tar_map(
      #### Pipeline: > NoFilters > WindoSize > MP Threshold > MP Constraints Branch ----
      list(map_mp_time_constraint = var_mp_time_constraint),
      #### Pipeline: > NoFilters > WindoSize > MP Threshold > MP Constraints > Compute the Matrix Profile from scratch as gold standard
      # tar_target(
      #   ds_stats_mps_nc,
      #   process_ts_in_file(dataset,
      #     id = "mps_raw",
      #     fun = compute_streaming_profile,
      #     params = list(
      #       window_size = map_window_size,
      #       ez = var_ez,
      #       progress = FALSE,
      #       batch = var_mp_batch,
      #       history = var_mp_history,
      #       mp_time_constraint = map_mp_time_constraint
      #     ),
      #     exclude = var_exclude
      #   ),
      #   pattern = map(dataset)
      # ),
      tar_target(
        ##### Pipeline: > NoFilters > WindoSize > MP Threshold > MP Constraints > Compute the Matrix Profile using Stats ----
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
          exclude = var_exclude
        ),
        pattern = map(dataset, ds_stats)
      ),
      tar_map(
        ##### Pipeline: > NoFilters > WindoSize > MP Threshold > MP Constraints + FLOSS Constraints Zero Branch ----
        list(map_floss_time_constraint = 0),
        tar_target(
          ##### Pipeline: > NoFilters > WindoSize > MP Threshold > MP Constraints + FLOSS Constraints Zero > Compute FLOSS ----
          ds_stats_mps_floss,
          process_ts_in_file(ds_stats_mps,
            id = "floss_mps",
            exclude = var_exclude,
            fun = compute_floss,
            params = list(
              window_size = map_window_size,
              ez = var_ez * map_window_size,
              mp_time_constraint = map_mp_time_constraint,
              floss_time_constraint = map_floss_time_constraint,
              history = var_mp_history,
              sample_freq = const_sample_freq
            )
          ),
          pattern = map(ds_stats_mps)
        ),
        l_regime_thr_root <- tar_map(
          ##### Pipeline: > NoFilters > WindoSize > MP Threshold > MP Constraints + FLOSS Constraints Zero > Regime Threshold Branch ----
          values = list(map_regime_threshold = var_regime_threshold),
          tar_target(
            ##### Pipeline: > NoFilters > WindoSize > MP Threshold > MP Constraints + FLOSS Constraints Zero > Regime Threshold > Extract regime changes ----
            regimes,
            process_ts_in_file(ds_stats_mps_floss,
              id = "regimes",
              fun = extract_regimes,
              params = list(
                window_size = map_window_size,
                ez = var_ez,
                regime_threshold = map_regime_threshold,
                regime_landmark = var_regime_landmark, # 3 sec from the end
                progress = FALSE,
                batch = var_mp_batch,
                history = var_mp_history,
                mp_time_constraint = map_mp_time_constraint,
                floss_time_constraint = map_floss_time_constraint
              ),
              exclude = var_exclude
            ),
            pattern = map(ds_stats_mps_floss)
          ),
          tar_target(
            ##### Pipeline: > NoFilters > WindoSize > MP Threshold > MP Constraints + FLOSS Constraints Zero > Regime Threshold > Extract regimes samples ----
            regimes_samples,
            process_ts_in_file(c(dataset, regimes),
              id = "regimes_samples",
              fun = extract_regime_sample,
              params = list(
                window_size = map_window_size,
                ez = var_ez,
                regime_threshold = map_regime_threshold,
                regime_landmark = var_regime_landmark, # 3 sec from the end
                progress = FALSE,
                batch = var_mp_batch,
                history = var_mp_history,
                mp_time_constraint = map_mp_time_constraint,
                floss_time_constraint = map_floss_time_constraint
              ),
              exclude = var_exclude
            ),
            pattern = map(dataset, regimes)
          )
          # tar_target(
          #   ##### Pipeline: > NoFilters > WindoSize > MP Threshold > MP Constraints + FLOSS Constraints Zero > Regime Threshold > Plot regime changes
          #   graph_regime,
          #   {
          #     tar_cancel(skip_graphics)
          #     process_ts_in_file(c(dataset, regimes),
          #       id = "plot_regimes",
          #       fun = plot_regimes,
          #       params = list(
          #         window_size = map_window_size,
          #         regime_threshold = map_regime_threshold,
          #         threshold = map_mp_threshold,
          #         history = var_mp_history,
          #         mp_time_constraint = map_mp_time_constraint,
          #         floss_time_constraint = map_floss_time_constraint,
          #         save_png = FALSE
          #       ),
          #       exclude = var_exclude
          #     )
          #   },
          #   pattern = map(dataset, regimes)
          # )
        )
      )
    ),
    l_no_mp_tconstr_root <- tar_map(
      #### Pipeline: > NoFilters > WindoSize > MP Threshold > MP Costraints Zero Branch ----
      list(map_mp_time_constraint = 0),
      tar_map(
        #### Pipeline: > NoFilters > WindoSize > MP Threshold > MP Costraints Zero + FLOSS Costraints Branch ----
        list(map_floss_time_constraint = var_floss_time_constraint),
        tar_target(
          ##### Pipeline: > NoFilters > WindoSize > MP Threshold > MP Costraints Zero + FLOSS Costraints > Compute FLOSS ----
          ds_stats_mps_nc_floss,
          process_ts_in_file(ds_stats_mps_nc,
            id = "floss",
            exclude = var_exclude,
            fun = compute_floss,
            params = list(
              window_size = map_window_size,
              ez = var_ez * map_window_size,
              mp_time_constraint = map_mp_time_constraint,
              floss_time_constraint = map_floss_time_constraint,
              history = var_mp_history,
              sample_freq = const_sample_freq
            )
          ),
          pattern = map(ds_stats_mps_nc)
        ),
        l_no_regime_thr_root <- tar_map(
          values = list(map_regime_threshold = var_regime_threshold),
          tar_target(
            ##### Pipeline: > NoFilters > WindoSize > MP Threshold > MP Costraints Zero + FLOSS Costraints > Regime Threshold > Extract regime changes ----
            regimes_nc,
            process_ts_in_file(ds_stats_mps_nc_floss,
              id = "regimes",
              fun = extract_regimes,
              params = list(
                window_size = map_window_size,
                ez = var_ez,
                regime_threshold = map_regime_threshold,
                regime_landmark = var_regime_landmark, # 3 sec from the end
                progress = FALSE,
                batch = var_mp_batch,
                history = var_mp_history,
                mp_time_constraint = map_mp_time_constraint,
                floss_time_constraint = map_floss_time_constraint
              ),
              exclude = var_exclude
            ),
            pattern = map(ds_stats_mps_nc_floss)
          ),
          tar_target(
            ##### Pipeline: > NoFilters > WindoSize > MP Threshold > MP Costraints Zero + FLOSS Costraints > Regime Threshold > Extract regimes samples ----
            regimes_samples_nc,
            process_ts_in_file(c(dataset, regimes_nc),
              id = "regimes_samples",
              fun = extract_regime_sample,
              params = list(
                window_size = map_window_size,
                ez = var_ez,
                regime_threshold = map_regime_threshold,
                regime_landmark = var_regime_landmark, # 3 sec from the end
                progress = FALSE,
                batch = var_mp_batch,
                history = var_mp_history,
                mp_time_constraint = map_mp_time_constraint,
                floss_time_constraint = map_floss_time_constraint
              ),
              exclude = var_exclude
            ),
            pattern = map(dataset, regimes_nc)
          )
          # tar_target(
          #   ##### Pipeline: > NoFilters > WindoSize > MP Threshold > MP Costraints Zero + FLOSS Costraints > Regime Threshold > Plot regime changes
          #   graph_regime_nc,
          #   {
          #     tar_cancel(skip_graphics)
          #     process_ts_in_file(c(dataset, regimes_nc),
          #       id = "plot_regimes_nc",
          #       fun = plot_regimes,
          #       params = list(
          #         window_size = map_window_size,
          #         regime_threshold = map_regime_threshold,
          #         threshold = map_mp_threshold,
          #         history = var_mp_history,
          #         mp_time_constraint = map_mp_time_constraint,
          #         floss_time_constraint = map_floss_time_constraint,
          #         save_png = FALSE
          #       ),
          #       exclude = var_exclude
          #     )
          #   },
          #   pattern = map(dataset, regimes_nc)
          # )
        )
      )
    )
  )
)
# #### Pipeline: > Filters Branch ----
# tar_map(
#   list(map_filter_w_size = var_filter_w_size),
# #### Pipeline: > Filters > Compute filters for noisy data ----
#   tar_target(
#     filters,
#     process_ts_in_file(dataset,
#       id = "filters",
#       fun = compute_filters,
#       params = list(
#         filter_w_size = map_filter_w_size,
#         cplx_lim = 8
#       ),
#       exclude = var_exclude
#     ),
#     pattern = map(dataset)
#   ),
#   #### Pipeline: > Filters > Apply Filter on Data ----
#   tar_target(
#     ds_filtered,
#     process_ts_in_file(c(dataset, filters),
#       id = "filter_data",
#       fun = filter_data,
#       params = list(
#         filter = "complex_lim"
#       ),
#       exclude = var_exclude
#     ),
#     pattern = map(dataset, filters)
#   ),
#   tar_map(
#     list(map_window_size = var_window_size),
#     #### Pipeline: > Filters > Compute Stats on Filtered Data ----
#     tar_target(
#       ds_filtered_stats,
#       process_ts_in_file(ds_filtered,
#         id = "comp_stats_filtered",
#         fun = compute_companion_stats,
#         params = list(
#           window_size = map_window_size,
#           n_workers = 1
#         ),
#         exclude = var_exclude
#       ),
#       pattern = map(ds_filtered)
#     ),
#     tar_map(
#       list(map_mp_time_constraint = var_mp_time_constraint),
#       #### Pipeline: > Filters > Compute the Matrix Profile With Filtered Data with Stats ----
#       tar_target(
#         ds_filtered_stats_mps,
#         process_ts_in_file(c(ds_filtered, ds_filtered_stats),
#           id = "mps_filtered_stats",
#           fun = compute_s_profile_with_stats,
#           params = list(
#             window_size = map_window_size,
#             ez = var_ez,
#             progress = FALSE,
#             batch = var_mp_batch,
#             history = var_mp_history,
#             mp_time_constraint = map_mp_time_constraint
#           ),
#           exclude = var_exclude
#         ),
#         pattern = map(ds_filtered, ds_filtered_stats)
#       ),
#       #### Pipeline: > Filters > Compute FLOSS ----
#       tar_target(
#         ds_filtered_stats_mps_floss,
#         process_ts_in_file(ds_filtered_stats_mps,
#           id = "floss",
#           exclude = var_exclude,
#           fun = compute_floss,
#           params = list(
#             window_size = map_window_size,
#             ez = var_ez * map_window_size,
#             history = var_mp_history,
#             mp_time_constraint = map_mp_time_constraint
#           )
#         ),
#         pattern = map(ds_filtered_stats_mps)
#       )
#     )
#   )
# )

#### Pipeline: Join targets ----
list(l_input, l_dataset, l_w_size_root)

#### Pipeline: End ----

# res <- test_set %>% keep(function(x) attr(x, "info")$true == TRUE)
# tar_make_future(workers = 4)
# tar_make_clustermq(workers = 4)


# tar_prune() # cleanup _targets folder keeping only the latest cache.
# tar_glimpse() # shows the simple plot, faster
# tar_visnetwork(exclude = starts_with('.')) # shows the interactive plot
# tar_visnetwork(TRUE, label = c("time", "size", "branches"), )
# tar_visnetwork(TRUE, names = !starts_with("graph"), exclude = starts_with('.'))
# tar_manifest() # returns a tibble with all steps
# tar_manifest(fields = NULL) # shows way more columns
# tar_renv() # helper function to assure renv capture all package dependencies
# tar_watch(targets_only = TRUE, label = c("time", "branches", "size")) # frontend to watch the building


# #### Compute the Right Matrix Profile Raw Data ----
# tar_target(
#   ds_mps,
#   process_ts_in_file(dataset,
#     id = "mps_raw",
#     fun = compute_streaming_profile,
#     params = list(
#       window_size = var_window_size,
#       ez = var_ez,
#       progress = FALSE,
#       batch = var_mp_batch,
#       history = var_mp_history,
#       mp_time_constraint = var_mp_time_constraint
#     ),
#     exclude = var_exclude
#   ),
#   pattern = cross(dataset, var_mp_time_constraint, var_window_size)
# ),
# ### Compute FLOSS ----
# tar_target(
#   ds_mps_floss,
#   process_ts_in_file(ds_mps,
#     id = "floss",
#     exclude = var_exclude,
#     fun = compute_floss,
#     params = list(
#       window_size = var_window_size,
#       ez = var_ez * var_window_size,
#       mp_time_constraint = var_mp_time_constraint,
#       history = var_mp_history
#     )
#   ),
#   pattern = map(cross(dataset, var_mp_time_constraint, var_window_size), ds_mps)
# ),

# #### Compute the Right Matrix Profile Filtered Data ----
# tar_target(
#   ds_filtered_mps,
#   process_ts_in_file(ds_filtered,
#     id = "mps_filtered",
#     fun = compute_streaming_profile,
#     params = list(
#       window_size = var_window_size,
#       ez = var_ez,
#       progress = FALSE,
#       batch = var_mp_batch,
#       history = var_mp_history,
#       mp_time_constraint = var_mp_time_constraint
#     ),
#     exclude = var_exclude
#   ),
#   pattern = cross(map(cross(var_window_size, dataset), ds_filtered), var_mp_time_constraint)
# ),
