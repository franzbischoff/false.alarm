library(targets)
library(tarchetypes)
# library(false.alarm)
# devtools::load_all(".")

# Load all scripts
script_files <- list.files(here::here("scripts"), pattern = "*.R")
sapply(here::here("scripts", script_files), source, .GlobalEnv)
rm(script_files)

options(tidyverse.quiet = TRUE)

tar_option_set(
  packages = "false.alarm",
  format = "rds",
  resources = list(compress = "xz"),
  garbage_collection = TRUE,
  error = "workspace"
  # iteration = "list",
  # debug = "read_ecg",
  # imports = "false.alarm" # TODO: remove when there is no change on package functions. Clears the graph.
)

options(clustermq.scheduler = "multiprocess")
options(target_dataset_path = "inst/extdata/physionet/")
future::plan(future::multisession) # tried at least on the split step and seems not worth it for now.
# debug(read_ecg)

debugme::debugme()

list(
  list(
    #### Read files from directory ----
    tar_files_input(
      file_paths,
      head(find_all_files(), 12),
      # batches = 2,
      # Use vector for filenames
      # iteration = "vector"
    ),
    tar_target(
      whole_dataset,
      read_ecg(file_paths),
      pattern = map(file_paths)
    ),
    #### Compute filters for noisy data ----
    tar_target(
      whole_dataset_filter,
      process_ts_in_file(whole_dataset,
        exclude = c("time", "ABP", "PLETH", "RESP"),
        fun = filter_data,
        params = list(
          attribute = "filters",
          window = 200,
          cplx_lim = 8
        )
      ),
      pattern = map(whole_dataset)
    ),
    #### Compute the Matrix Profile ----
    tar_target(
      whole_dataset_mp,
      process_ts_in_file(whole_dataset_filter,
        exclude = c("time", "ABP", "PLETH", "RESP"),
        fun = compute_matrix_profile,
        params = list(
          attribute = "mp",
          window_size = 200, ez = 0.5, idxs = TRUE,
          s_size = 1.0, progress = FALSE,
          distance = "euclidean",
          constraint = 20 * 250
        )
      ),
      pattern = map(whole_dataset_filter)
    ),
    ### Apply Filter on MP ----
    tar_target(
      whole_dataset_mp_filtered,
      process_ts_in_file(whole_dataset_mp,
        exclude = c("time", "ABP", "PLETH", "RESP"),
        fun = filter_mp,
        params = list(
          attribute = "mp",
          filter = "complex_lim"
        )
      ),
      pattern = map(whole_dataset_mp)
    ),
    #### Compute the FLUSS Arcs ----
    tar_target(
      whole_dataset_mp_arcs,
      compute_arcs(whole_dataset_mp_filtered,
        exclude = c("time", "ABP", "PLETH", "RESP"),
        time_constraint = 20 * 250
      ), # 30s 250hz
      pattern = map(whole_dataset_mp_filtered)
    ),
    #### Extract Regimes ----
    tar_target(
      whole_dataset_mp_fluss,
      fluss_extract(whole_dataset_mp_arcs,
        exclude = c("time", "ABP", "PLETH", "RESP"),
        num_segments = 5,
        exclusion_zone = 25
      ),
      pattern = map(whole_dataset_mp_arcs)
    ),
    tar_target(
      fluss_plots,
      plot_fluss(whole_dataset_mp_fluss),
      pattern = map(whole_dataset_mp_fluss)
    )
  )

  # # Static branch for split_values
  # tar_map(
  #   values = list(split_value = 0.80),
  #   #### Split is last thing, to avoid spurious computations ----
  #   tar_rep(
  #     idxs,
  #     make_idxs_split(length(whole_dataset_mp), split_value),
  #     batches = 5, # branches
  #   ),
  #   tar_target(
  #     training_set,
  #     whole_dataset_mp[idxs$training],
  #     map(idxs)
  #   ),
  #   tar_target(
  #     test_set,
  #     whole_dataset_mp[idxs$test],
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
# tar_watch() # frontend to watch the building
