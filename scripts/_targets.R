source(here("scripts", "_globals.R"))

#### Pipeline: Start ----

r_input <- tar_files_input(
  #### Pipeline: Read files from directory ----
  file_paths,
  # tail(head(find_all_files(types = "all"), var_head), 10),
  find_all_files(types = "asystole") # , "bradycardia", "tachycardia", "vfib", "vtachy"
)
r_dataset <- tar_target(
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
      exclude = var_exclude
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
      # This will create a "common" branch where no constraint is applyed and one branch with mp constraints and other with floss constraints.
      # Seems complicated here, but has simplified the pileline a lot.
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
      #     exclude = var_exclude
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
          exclude = var_exclude
        ),
        pattern = map(dataset, ds_stats)
      ),
      tar_target(
        ##### Pipeline: > NoFilters > WindowSize > MP Threshold > MP/FLOSS Constraints > Compute FLOSS ----
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
            exclude = var_exclude
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
        #       exclude = var_exclude
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
#         exclude = var_exclude
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
list(r_input, r_dataset, b_window_sizes)

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
