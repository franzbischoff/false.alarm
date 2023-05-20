get_minimum_cacs <- function(training_set, params) {
  "!DEBUG Computing profiles."
  profiles <- purrr::map(training_set, function(x) {
    compute_streaming_profile(x, list(
      window_size = params$window_size,
      ez = params$ez,
      progress = params$progress,
      batch = params$batch,
      history = params$history,
      mp_time_constraint = params$mp_time_constraint
    ))
  })

  "!DEBUG Computing FLOSS."
  floss <- purrr::map(profiles, function(x) {
    compute_floss(x, list(
      window_size = params$window_size,
      ez = params$ez * params$window_size,
      mp_time_constraint = params$mp_time_constraint,
      history = params$history,
      sample_freq = params$sample_freq
    ))
  })

  "!DEBUG Retrieving minimums."
  if (params$mp_time_constraint > 0) {
    cac_left_idx <- (params$history - params$mp_time_constraint)
  } else {
    cac_left_idx <- params$history / 2
  }
  cac_size <- params$history - params$window_size + 1

  checkmate::assert_true(cac_size > cac_left_idx)

  min_global <- NULL

  minimums <- purrr::map(floss, function(x) {
    min_cac <- Inf
    min_local <- NULL
    idx_local <- NULL
    for (i in seq_along(x)) {
      mm <- min(x[[i]]$cac[seq.int(cac_left_idx, cac_size)])

      if (mm < min_cac) {
        min_cac <- mm
        min_local <- c(min_local, mm)
        idx_local <- c(idx_local, i)
      }
    }

    min_local <- tail(min_local, 10)
    idx_local <- tail(idx_local, 10)

    min_cacs <- x[idx_local]
    attr(min_cacs, "values") <- min_local

    min_global <<- c(min_global, min(min_local))

    # test <- min_cacs |>
    #   purrr::flatten() |>
    #   purrr::keep(function(x) length(x) > 1)
    min_cacs
  })

  if (params$only_mins) {
    return(min_global)
  } else {
    return(list(floss = floss, minimums = minimums, mins = min_global))
  }
}
