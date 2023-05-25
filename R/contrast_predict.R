contrast_predict <- function(contrast_list, window_size, time_constraint, regime_threshold,
                             regime_landmark,
                             ez = 0.5, history = 5000L,
                             sample_freq = 250L, batch = 100L) {
  regimes <- contrast_extract(contrast_list,
    params = list(
      window_size = window_size,
      ez = ez,
      regime_threshold = regime_threshold,
      regime_landmark = floor(regime_landmark * sample_freq), # 3L sec from the end
      batch = batch,
      history = history,
      mp_time_constraint = time_constraint,
      contrast_time_constraint = 0L
    ),
    infos = list(foo = "bar")
  )

  if (isFALSE(regimes)) {
    return(1L)
  } else {
    return(regimes$idxs)
  }
}


contrast_extract <- function(contrast_list, params, infos) {
  checkmate::qassert(contrast_list, "L+")
  checkmate::qassert(params, "L+")

  info <- attr(contrast_list, "info")
  pars <- attr(contrast_list, "params")
  # subset_start <- ifelse(isFALSE(info$subset), 0L, info$subset[1L] - 1L)

  checkmate::assert_true(
    all(
      pars$window_size == params$window_size,
      pars$mp_time_constraint == params$mp_time_constraint,
      pars$history == params$history
    )
  )

  regime_threshold <- params$regime_threshold
  # window_size <- params$window_size
  history <- params$history
  landmark <- history - params$regime_landmark # here is where we look for the minimum value

  if (params$mp_time_constraint > 0L && params$mp_time_constraint <= floor(params$history * 3.0 / 4.0)) {
    contrast_constraint <- params$mp_time_constraint
  } else {
    contrast_constraint <- floor(history / 2.0)
  }

  "!DEBUG extract regimes."
  current_abs_min_idx <- 0L
  current_abs_min_value <- 1.0

  all_regimes_idxs <- NULL
  all_regimes_values <- NULL

  # TODO: Review if landmark corresponds in cac the index of data

  # iterates over all cacs of this time series
  purrr::map(contrast_list, function(x) {
    cac <- x$cac
    # cac[cac > regime_threshold] <- 1.0
    # cac[seq.int(1L, history - (4L * 250L))] <- 1.0
    cac[seq.int(1L, history - contrast_constraint)] <- 1.0
    # min_trigger_idx <- which.min(cac)
    # if (min_trigger_idx > landmark) {
    #   rlang::inform("min_trigger at ", min_trigger_idx, " for landmark ", landmark, ".")
    # }

    if (cac[landmark] < regime_threshold) {
      abs_min_idx <- x$offset - history + landmark + 1L
      if ((abs_min_idx - current_abs_min_idx) > contrast_constraint) {
        # IMPROVE: tweak contrast_constraint
        # cli::cli_inform("abs_min_idx at {abs_min_idx}, value {cac[landmark]}.")
        current_abs_min_idx <<- abs_min_idx
        current_abs_min_value <<- cac[landmark]
        all_regimes_idxs <<- c(all_regimes_idxs, current_abs_min_idx)
        all_regimes_values <<- c(all_regimes_values, current_abs_min_value)
      }
      if (cac[landmark] < current_abs_min_value) {
        if ((abs_min_idx - current_abs_min_idx) < floor(history / 2.0)) {
          # IMPROVE: tweak floor(history / 2)
          # cli::cli_inform("abs_min_idx2 at {abs_min_idx}, value {cac[landmark]}.")
          current_abs_min_idx <<- abs_min_idx
          current_abs_min_value <<- cac[landmark]
          all_regimes_idxs <<- c(all_regimes_idxs, current_abs_min_idx)
          all_regimes_values <<- c(all_regimes_values, current_abs_min_value)
        }
      }
    }

    # if (min_trigger_idx > (history - contrast_constraint)) {
    #   min_trigger_abs_idx <- min_trigger_idx + x$offset - history
    #   "!DEBUG triggered at `min_trigger_abs_idx`."
    #   min_trigger_value <- x$cac[min_trigger_idx]

    #   if ((min_trigger_abs_idx - current_abs_min_idx) < contrast_constraint) {
    #     # updates the current trigger/min
    #     if (min_trigger_value < current_abs_min_value) {
    #       current_abs_min_idx <<- min_trigger_abs_idx
    #       current_abs_min_value <<- min_trigger_value
    #     }
    #   } else {
    #     current_abs_min_idx <<- min_trigger_abs_idx
    #     current_abs_min_value <<- min_trigger_value
    #   }
    # }
    x
  })

  if (!is.null(all_regimes_idxs)) {
    regimes <- tibble::as_tibble(list(idxs = all_regimes_idxs, values = all_regimes_values))
    regimes <- dplyr::distinct(regimes, idxs, .keep_all = TRUE) |> dplyr::arrange(idxs) # nolint
  } else {
    regimes <- FALSE
  }

  attr(regimes, "info") <- info
  attr(regimes, "params") <- params

  return(regimes)
}
