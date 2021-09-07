library(targets)
library(tarchetypes)
library(purrr)

dev_mode <- TRUE # !identical(Sys.getenv("DEBUGME"), "")
cluster <- FALSE
backend <- "FUTURE"

if (dev_mode) {
  # I know I shall not use this
  # devtools::load_all(".")
  # Sys.setenv(DEBUGME_OUTPUT_FILE = "debugme.log")
}

# examples: a109l_aVF_300_0_raw
#           a161l (asystole) 300_1250, false neg

# Load all scripts
script_files <- list.files(here::here("scripts"), pattern = "*.R")
sapply(here::here("scripts", script_files), source, local = .GlobalEnv, encoding = "UTF-8")
rm(script_files)

options(tidyverse.quiet = TRUE)
options(target_ds_path = "inst/extdata/physionet/")

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

# use renv::install(".") to update the rcpp functions
tar_option_set(
  packages = "false.alarm",
  format = "rds",
  resources = tar_resources(
    #   #   #   # *** If using clustermq for multithreading / clustering ***
    clustermq = tar_resources_clustermq(
      template = list(num_cores = 4)
    ), # or n_jobs??
    #   #   #   # *** If using future for multithreading / clustering ***
    future = tar_resources_future(
      resources = list(num_cores = 4)
    )
  ),
  garbage_collection = TRUE,
  workspace_on_error = TRUE,
  # memory = "transient",
  # storage = "main",
  # envir = globalenv(),
  # iteration = "list",
  # debug = "process_ts_in_file"
  imports = "false.alarm" # TODO: remove when there is no change on package functions. Clears the graph.
)

var_head <- 10
var_exclude <- c("time", "V", "ABP", "PLETH", "RESP")
var_mp_batch <- 100
var_mp_constraint <- 20 * 250 # 20 secs
var_window_size <- c(300, 500, 750) # c(200, 250, 300)
var_time_constraint <- c(0, 5 * 250, 10 * 250, 17 * 250)
var_filter_w_size <- 100 # c(100, 200)
var_ez <- 0.5
var_subset <- FALSE # 55001:75000 # last 80 secs


# debug(process_ts_in_file)
# tar_make(names = ds_mp_filtered, callr_function = NULL)

# start debugme after loading all functions
if (dev_mode) {
  debugme::debugme()
}

list(
  #### Read files from directory ----
  tar_files_input(
    file_paths,
    tail(head(find_all_files(), var_head), 1),
    # batches = 2,
    # Use vector for filenames
    # iteration = "vector"
  ),
  tar_target(
    dataset,
    read_ecg(file_paths, subset = var_subset),
    pattern = map(file_paths)
  ),
  tar_target(
    neg_training_floss,
    create_floss_training()
  ),
  # without filter
  tar_map(
    list(map_window_size = var_window_size),
    #### Compute Stats ----
    tar_target(
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
    tar_map(
      list(map_time_constraint = var_time_constraint),
      tar_target(
        floss_threshold,
        get_minimum_cacs(
          neg_training_floss,
          list(
            batch = var_mp_batch,
            history = var_mp_constraint,
            window_size = map_window_size,
            time_constraint = map_time_constraint,
            ez = var_ez,
            only_mins = TRUE,
            progress = FALSE
          )
        )
      ),
      #### Compute the Matrix Profile With Stats ----
      tar_target(
        ds_stats_mps,
        process_ts_in_file(c(dataset, ds_stats),
          id = "mps_raw_stats",
          fun = compute_s_profile_with_stats,
          params = list(
            window_size = map_window_size,
            ez = var_ez,
            progress = FALSE,
            batch = var_mp_batch,
            history = var_mp_constraint,
            time_constraint = map_time_constraint
          ),
          exclude = var_exclude
        ),
        pattern = map(dataset, ds_stats)
      ),
      ### Compute FLOSS ----
      tar_target(
        ds_stats_mps_floss,
        process_ts_in_file(ds_stats_mps,
          id = "floss",
          exclude = var_exclude,
          fun = compute_floss,
          params = list(
            window_size = map_window_size,
            ez = var_ez * map_window_size,
            time_constraint = map_time_constraint,
            history = var_mp_constraint,
            threshold = TRUE
          )
        ),
        pattern = map(ds_stats_mps)
      ),
      ### Extract regime changes ----
      tar_target(
        regimes,
        process_ts_in_file(ds_stats_mps_floss,
          id = "regimes",
          fun = extract_regimes,
          params = list(
            window_size = map_window_size,
            ez = var_ez,
            min_cac = floss_threshold,
            progress = FALSE,
            batch = var_mp_batch,
            history = var_mp_constraint,
            time_constraint = map_time_constraint
          ),
          exclude = var_exclude
        ),
        pattern = map(ds_stats_mps_floss)
      )
    )
  )
  # # with filters
  # tar_map(
  #   list(map_filter_w_size = var_filter_w_size),
  #   # #### Compute filters for noisy data ----
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
  #   # ### Apply Filter on Data ----
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
  #     #### Compute Stats on Filtered Data ----
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
  #       list(map_time_constraint = var_time_constraint),
  #       #### Compute the Matrix Profile With Filtered Data with Stats ----
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
  #             history = var_mp_constraint,
  #             time_constraint = map_time_constraint
  #           ),
  #           exclude = var_exclude
  #         ),
  #         pattern = map(ds_filtered, ds_filtered_stats)
  #       ),
  #       ### Compute FLOSS ----
  #       tar_target(
  #         ds_filtered_stats_mps_floss,
  #         process_ts_in_file(ds_filtered_stats_mps,
  #           id = "floss",
  #           exclude = var_exclude,
  #           fun = compute_floss,
  #           params = list(
  #             window_size = map_window_size,
  #             ez = var_ez * map_window_size,
  #             history = var_mp_constraint,
  #             time_constraint = map_time_constraint,
  #             threshold = FALSE
  #           )
  #         ),
  #         pattern = map(ds_filtered_stats_mps)
  #       )
  #     )
  #   )
  # )
)


# res <- test_set %>% keep(function(x) attr(x, "info")$true == TRUE)
# tar_make_future(workers = 4)
# tar_make_clustermq(workers = 4)


# tar_prune() # cleanup _targets folder keeping only the latest cache.
# tar_glimpse() # shows the simple plot, faster
# tar_visnetwork(exclude = starts_with('.')) # shows the interactive plot
# tar_visnetwork(TRUE, label = c("time", "size", "branches"))
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
#       history = var_mp_constraint,
#       time_constraint = var_time_constraint
#     ),
#     exclude = var_exclude
#   ),
#   pattern = cross(dataset, var_time_constraint, var_window_size)
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
#       time_constraint = var_time_constraint,
#       history = var_mp_constraint,
#       threshold = FALSE
#     )
#   ),
#   pattern = map(cross(dataset, var_time_constraint, var_window_size), ds_mps)
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
#       history = var_mp_constraint,
#       time_constraint = var_time_constraint
#     ),
#     exclude = var_exclude
#   ),
#   pattern = cross(map(cross(var_window_size, dataset), ds_filtered), var_time_constraint)
# ),
