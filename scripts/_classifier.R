source(here("scripts", "_globals.R"))

#### Pipeline: Start ----

r_dataset <- tar_target(dataset, tar_read(dataset, store = "_targets"))

#### Pipeline: > NoFilters Branch ----
b_window_sizes <- tar_target(
  ##### Pipeline: > NoFilters > WindowSize > Compute Stats ----
  ds_stats,
  process_ts_in_file(dataset,
    id = "comp_stats",
    fun = compute_companion_stats,
    params = list(
      window_size = 100,
      n_workers = 1
    ),
    exclude = var_exclude
  ),
  pattern = map(dataset)
)

#### Pipeline: Join targets ----
list(r_dataset, b_window_sizes)

#### Pipeline: End ----
