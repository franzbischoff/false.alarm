

training_regimes <- function(ecg_data, window_size, mp_threshold, time_constraint,
                             ez = 0.5, history = 5000, sample_freq = 250, batch = 100) {
  "!DEBUG processing Stats"

  stats <- compute_companion_stats(ecg_data,
    list(window_size = window_size, n_workers = 1),
    infos = list(foo = "bar")
  )

  "!DEBUG processing MP"

  mp <- compute_s_profile_with_stats(list(ecg_data, stats),
    params = list(
      window_size = window_size,
      history = history, threshold = mp_threshold,
      mp_time_constraint = time_constraint, progress = FALSE,
      ez = ez, batch = batch
    ),
    infos = list(foo = "bar")
  )

  rm(stats)
  gc(verbose = FALSE)

  "!DEBUG processing FLOSS"

  floss <- compute_floss(mp,
    params = list(
      window_size = window_size,
      ez = round(ez * window_size + .Machine$double.eps^0.5),
      mp_time_constraint = time_constraint,
      floss_time_constraint = 0,
      cac_only = TRUE,
      history = history,
      sample_freq = sample_freq
    ),
    infos = list(foo = "bar")
  )
  rm(mp)
  gc(verbose = FALSE)

  floss
}
