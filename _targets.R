library(targets)
library(tarchetypes)
library(purrr)

dev_mode <- !identical(Sys.getenv("DEBUGME"), "")
cluster <- FALSE
backend <- "CLUSTERMQ"

if (dev_mode) {
  # I know I shall not use this
  # devtools::load_all(".")
  # Sys.setenv(DEBUGME_OUTPUT_FILE = "debugme.log")
}

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
          batchtools::Worker$new("franz@192.168.1.237", ncpus = 4)
          # batchtools::Worker$new("localhost", ncpus = 4)
        ),
        fs.latency = 1000
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
    clustermq = tar_resources_clustermq(template = list(num_cores = 4)) # or n_jobs??
    #   #   #   # *** If using future for multithreading / clustering ***
    # future = tar_resources_future(
    #   resources = list(num_cores = 4)
    # )
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

var_head <- 3
var_exclude <- c("time", "ABP", "PLETH", "RESP")
var_mp_batch <- 100
var_mp_constraint <- 20 * 250 # 20 secs
var_time_constraint <- 5 * 250 # 5 secs
var_window_size <- 200
var_ez <- 0.5

# debug(process_ts_in_file)
# tar_make(names = ds_mp_filtered, callr_function = NULL)

# start debugme after loading all functions
if (dev_mode) {
  debugme::debugme()
}

list(
  list(
    #### Read files from directory ----
    tar_files_input(
      file_paths,
      head(find_all_files(), var_head),
      # batches = 2,
      # Use vector for filenames
      # iteration = "vector"
    ),
    tar_target(
      dataset,
      read_ecg(file_paths),
      pattern = map(file_paths)
    ),
    # #### Compute filters for noisy data ----
    tar_target(
      filters,
      process_ts_in_file(dataset,
        id = "filters",
        fun = compute_filters,
        params = list(
          window_size = var_window_size,
          cplx_lim = 8
        ),
        exclude = var_exclude
      ),
      pattern = map(dataset)
    ),
    #### Compute Stats ----
    tar_target(
      ds_stats,
      process_ts_in_file(dataset,
        id = "comp_stats",
        fun = compute_companion_stats,
        params = list(
          window_size = var_window_size,
          n_workers = 1
        ),
        exclude = var_exclude
      ),
      pattern = map(dataset)
    ),
    ### Apply Filter on Data ----
    tar_target(
      ds_filtered,
      process_ts_in_file(c(dataset, filters),
        id = "filter_data",
        fun = filter_data,
        params = list(
          filter = "complex_lim"
        ),
        exclude = var_exclude
      ),
      pattern = map(dataset, filters)
    ),
    #### Compute Stats on Filtered Data ----
    tar_target(
      ds_stats_filtered,
      process_ts_in_file(ds_filtered,
        id = "comp_stats_filtered",
        fun = compute_companion_stats,
        params = list(
          window_size = var_window_size,
          n_workers = 1
        ),
        exclude = var_exclude
      ),
      pattern = map(ds_filtered)
    ),
    #### Compute the Matrix Profile With Stats ----
    tar_target(
      ds_stats_mps,
      process_ts_in_file(c(dataset, ds_stats),
        id = "mps_raw_stats",
        fun = compute_s_profile_with_stats,
        params = list(
          window_size = var_window_size,
          ez = var_ez,
          progress = FALSE,
          batch = var_mp_batch,
          history = var_mp_constraint,
          time_constraint = var_time_constraint
        ),
        exclude = var_exclude
      ),
      pattern = map(dataset, ds_stats)
    ),
    #### Compute the Right Matrix Profile Raw Data ----
    tar_target(
      ds_mps,
      process_ts_in_file(dataset,
        id = "mps_raw",
        fun = compute_streaming_profile,
        params = list(
          window_size = var_window_size,
          ez = var_ez,
          progress = FALSE,
          batch = var_mp_batch,
          history = var_mp_constraint,
          time_constraint = var_time_constraint
        ),
        exclude = var_exclude
      ),
      pattern = map(dataset)
    ),
    #### Compute the Right Matrix Profile Filtered Data ----
    tar_target(
      ds_filtered_mps,
      process_ts_in_file(ds_filtered,
        id = "mps_filtered",
        fun = compute_streaming_profile,
        params = list(
          window_size = var_window_size,
          ez = var_ez,
          progress = FALSE,
          batch = var_mp_batch,
          history = var_mp_constraint,
          time_constraint = var_time_constraint
        ),
        exclude = var_exclude
      ),
      pattern = map(ds_filtered)
    ),
    #### Compute the Matrix Profile With Filtered Data with Stats ----
    tar_target(
      ds_stats_filtered_mps,
      process_ts_in_file(c(ds_filtered, ds_stats_filtered),
        id = "mps_filtered_stats",
        fun = compute_s_profile_with_stats,
        params = list(
          window_size = var_window_size,
          ez = var_ez,
          progress = FALSE,
          batch = var_mp_batch,
          history = var_mp_constraint,
          time_constraint = var_time_constraint
        ),
        exclude = var_exclude
      ),
      pattern = map(ds_filtered, ds_stats_filtered)
    ),
    ### Apply Filter on MP ----
    tar_target(
      ds_stats_mps_floss,
      process_ts_in_file(ds_stats_mps,
        id = "floss_constr",
        exclude = var_exclude,
        fun = compute_floss,
        params = list(
          ez = var_ez * var_window_size,
          time_constraint = var_time_constraint,
          threshold = FALSE
        )
      ),
      pattern = map(ds_stats_mps)
    ),
    ### Apply Filter on MP ----
    tar_target(
      ds_stats_mps_floss2,
      process_ts_in_file(ds_stats_mps,
        id = "floss_no_constr",
        exclude = var_exclude,
        fun = compute_floss,
        params = list(
          ez = var_ez * var_window_size,
          time_constraint = 0,
          threshold = FALSE
        )
      ),
      pattern = map(ds_stats_mps)
    ),
    ### Apply Filter on MP ----
    tar_target(
      ds_stats_mps_floss3,
      process_ts_in_file(ds_stats_mps,
        id = "floss_const_thr",
        exclude = var_exclude,
        fun = compute_floss,
        params = list(
          ez = var_ez * var_window_size,
          time_constraint = var_time_constraint,
          threshold = TRUE
        )
      ),
      pattern = map(ds_stats_mps)
    ),
    ### Apply Filter on MP ----
    tar_target(
      ds_stats_mps_floss4,
      process_ts_in_file(ds_stats_mps,
        id = "floss_no_const_thr",
        exclude = var_exclude,
        fun = compute_floss,
        params = list(
          ez = var_ez * var_window_size,
          time_constraint = 0,
          threshold = TRUE
        )
      ),
      pattern = map(ds_stats_mps)
    )

    # ### Apply Filter on MP ----
    # tar_target(
    #   ds_mp_filtered,
    #   process_ts_in_file(ds_mp,
    #     exclude = var_exclude,
    #     fun = filter_mp,
    #     params = list(
    #       attribute = "mp",
    #       filter = "complex_lim"
    #     )
    #   ),
    #   pattern = map(ds_mp)
    # ),
    # #### Compute the FLUSS Arcs ----
    # tar_target(
    #   ds_mp_arcs,
    #   compute_arcs(ds_mp_filtered,
    #     exclude = var_exclude,
    #     time_constraint = var_mp_constraint
    #   ), # 30s 250hz
    #   pattern = map(ds_mp_filtered)
    # )
    # #### Extract Regimes ----
    # tar_target(
    #   ds_mp_fluss,
    #   fluss_extract(ds_mp_arcs,
    #     exclude = var_exclude,
    #     num_segments = 5,
    #     exclusion_zone = 25
    #   ),
    #   pattern = map(ds_mp_arcs)
    # ),
    # tar_target(
    #   fluss_plots,
    #   plot_fluss(ds_mp_fluss),
    #   pattern = map(ds_mp_fluss)
    # )
  )

  # # Static branch for split_values
  # tar_map(
  #   values = list(split_value = 0.80),
  #   #### Split is last thing, to avoid spurious computations ----
  #   tar_rep(
  #     idxs,
  #     make_idxs_split(length(ds_mp), split_value),
  #     batches = 5, # branches
  #   ),
  #   tar_target(
  #     training_set,
  #     ds_mp[idxs$training],
  #     map(idxs)
  #   ),
  #   tar_target(
  #     test_set,
  #     ds_mp[idxs$test],
  #     map(idxs)
  #   ),
  #
  #   #### Now fit the model ----
  #
  #   tar_target(
  #     model,
  #     fit_model(training_set), ## fit(training_set) return the model
  #     map(training_set)
  #   ),
  #
  #   #### Now test the model ----
  #
  #   tar_target(
  #     results,
  #     test_model(model, test_set), ## test(model, test_set) return the results
  #     map(test_set)
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
