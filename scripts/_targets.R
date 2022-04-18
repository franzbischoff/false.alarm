# Sys.setenv(TAR_PROJECT = "main")

source(here("scripts", "_globals.R"))

#### Targets: Setup engine ----

cluster <- FALSE
backend <- "FUTURE"
workers <- 20

# if (isFALSE(cluster)) { ## Locally
#   if (backend == "FUTURE") {
#     library(future)
#     library(future.callr)
#     # list(plan(callr), plan(callr))
#   } else {
#     options(
#       clustermq.scheduler = "multiprocess",
#       clustermq.ssh.host = NULL,
#       clustermq.ssh.log = NULL
#     )
#     library(clustermq)
#   }
# } else {
#   if (backend == "FUTURE") { ## cluster # tar_make_future(workers = 4)
#     # *** If using future for multithreading / clustering ***
#     library(future)
#     library(future.batchtools)

#     future::plan(
#       strategy = future.batchtools::batchtools_custom,
#       cluster.functions = batchtools::makeClusterFunctionsSSH(
#         list(
#           # batchtools::Worker$new("franz@192.168.1.237", ncpus = 4)
#           batchtools::Worker$new("localhost", ncpus = workers)
#         ),
#         # fs.latency = 1000
#       )
#     )
#   } else { # tar_make_clustermq(workers = 3)
#     options(
#       clustermq.scheduler = "ssh",
#       clustermq.ssh.host = "franz@192.168.1.237", # use your user and host, obviously
#       clustermq.ssh.log = "~/cmq_ssh.log" # log for easier debugging # nolint
#     )
#     library(clustermq)
#   }
# }

#### Targets: Define targets options ----

# use renv::install(".") to update the rcpp functions
tar_option_set(
  packages = c("dplyr", "false.alarm"),
  format = "rds",
  # resources = tar_resources(
  #   #   #   #   # *** If using clustermq for multithreading / clustering ***
  #   clustermq = tar_resources_clustermq(
  #     template = list(num_cores = workers)
  #   ) # or n_jobs??
  #   #   #   #   # *** If using future for multithreading / clustering ***
  #   # future = tar_resources_future(
  #   #   # plan = future::plan(future.callr::callr),
  #   #   resources = list(n_cores = workers)
  #   # )
  # ),
  # garbage_collection = TRUE,
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

# start debugme after loading all functions
if (dev_mode) {
  debugme::debugme()
}

library(future)
library(future.callr)
plan(multicore)


#### Pipeline: Start ----

r_input <- tar_files_input(
  #### Pipeline: Read files from directory ----
  file_paths,
  # tail(head(find_all_files(classes = "all"), var_head), 10),
  find_all_files(classes = var_classes_include)
)
r_dataset <- tar_target(
  #### Pipeline: Import Files to R and Select Datasets ----
  dataset,
  read_and_prepare_ecgs(file_paths,
    subset = seq.int(45000, 75000),
    # true_alarm = TRUE,
    limit_per_class = 15
  )
)

# tar_load(dataset); tar_load(filters2); tar_load(filters);
# i <- 3; data <- dataset[[i]]$II; attr(data, "filters") <- filters[[i]]$II; plot_ecg2(data); data2 <- dataset[[i]]$II; attr(data2, "filters") <- filters2[[i]]$II; plot_ecg2(data2)
# i <- 3; data2 <- dataset[[i]]$II; attr(data2, "filters") <- filters2[[i]]$II; plot_ecg2(data2)

r_filters <- tar_target(
  filters,
  process_ts_in_file(dataset,
    id = "filters",
    fun = compute_filters,
    params = list(
      window_size = 250,
      filter_w_size = 100,
      cplx_lim = 8
    ),
    exclude = var_signals_exclude
  ),
  pattern = map(dataset)
)

r_filters2 <- tar_target(
  filters2,
  process_ts_in_file(dataset,
    id = "filters2",
    fun = compute_filters,
    params = list(
      window_size = 250,
      filter_w_size = 200,
      cplx_lim = 8
    ),
    exclude = var_signals_exclude
  ),
  pattern = map(dataset)
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
#     exclude = var_signals_exclude
#   ),
#   pattern = map(dataset)
# )
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
    pattern = map(dataset)
  ),
  b_mp_threshold <- tar_map(
    #### Pipeline: > NoFilters > WindowSize > MP Threshold Branch ----
    values = list(map_mp_threshold = var_mp_threshold),
    b_mp_floss_constraints <- tar_map(
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
      #### Pipeline: > NoFilters > WindowSize > MP Threshold > MP/FLOSS Constraints > Compute the Matrix Profile from scratch as gold standard
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
      #     exclude = var_signals_exclude
      #   ),
      #   pattern = map(dataset)
      # ),
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
        pattern = map(ds_stats_mps)
      ),
      b_regime_threshold <- tar_map(
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
          pattern = map(ds_stats_mps_floss)
        ),
        tar_target(
          ##### Pipeline: > NoFilters > WindowSize > MP Threshold > MP/FLOSS Constraints > Regime Threshold > Extract regimes samples ----
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
            exclude = var_signals_exclude
          ),
          pattern = map(dataset, regimes)
        )
        # tar_target(
        #   ##### Pipeline: > NoFilters > WindowSize > MP Threshold > MP/FLOSS Constraints > Regime Threshold > Plot regime changes
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
        #       exclude = var_signals_exclude
        #     )
        #   },
        #   pattern = map(dataset, regimes)
        # )
      )
    )
  ),
  combined_var <- tar_combine(combined_samples, b_mp_threshold[grepl("regimes_samples", names(b_mp_threshold))], command = list(!!!.x))
)

# #### Pipeline: > Filters Branch ----
# tar_map(
#   values = list(map_filter_w_size = var_filter_w_size),
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
#       exclude = var_signals_exclude
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
#       exclude = var_signals_exclude
#     ),
#     pattern = map(dataset, filters)
#   ),
#   tar_map(
#     values = list(map_window_size = var_window_size),
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
#         exclude = var_signals_exclude
#       ),
#       pattern = map(ds_filtered)
#     ),
#     tar_map(
#       values = list(map_mp_time_constraint = var_mp_time_constraint),
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
#           exclude = var_signals_exclude
#         ),
#         pattern = map(ds_filtered, ds_filtered_stats)
#       ),
#       #### Pipeline: > Filters > Compute FLOSS ----
#       tar_target(
#         ds_filtered_stats_mps_floss,
#         process_ts_in_file(ds_filtered_stats_mps,
#           id = "floss",
#           exclude = var_signals_exclude,
#           fun = compute_floss,
#           params = list(
#             window_size = map_window_size,
#             ez = round(var_ez * map_window_size + .Machine$double.eps^0.5),
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
list(r_input, r_dataset, r_filters, r_filters2, b_window_sizes)

#### Pipeline: End ----

# res <- test_set %>% keep(function(x) attr(x, "info")$true == TRUE)
# tar_make_future(workers = 4)
# tar_make_clustermq(workers = 4)


# tar_prune() # cleanup _targets folder keeping only the latest cache.
# tar_glimpse() # shows the simple plot, faster
# tar_visnetwork(exclude = starts_with('.')) # shows the interactive plot
# tar_visnetwork(TRUE, label = c("time", "size", "branches"))
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
#     exclude = var_signals_exclude
#   ),
#   pattern = cross(dataset, var_mp_time_constraint, var_window_size)
# ),
# ### Compute FLOSS ----
# tar_target(
#   ds_mps_floss,
#   process_ts_in_file(ds_mps,
#     id = "floss",
#     exclude = var_signals_exclude,
#     fun = compute_floss,
#     params = list(
#       window_size = var_window_size,
#       ez = round(var_ez * var_window_size + .Machine$double.eps^0.5),
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
#     exclude = var_signals_exclude
#   ),
#   pattern = cross(map(cross(var_window_size, dataset), ds_filtered), var_mp_time_constraint)
# ),
