extract_regimes <- function(floss_list, params) {
  checkmate::qassert(floss_list, "L+")
  checkmate::qassert(params, "L+")

  info <- attr(floss_list, "info")
  pars <- attr(floss_list, "params")
  # subset_start <- ifelse(isFALSE(info$subset), 0, info$subset[1] - 1)

  checkmate::assert_true(
    all(
      pars$window_size == params$window_size,
      pars$time_constraint == params$time_constraint,
      pars$history == params$history
    )
  )

  # TODO: compute the min cac
  min_cac <- params$min_cac
  window_size <- params$window_size
  history <- params$history
  landmark <- history - params$floss_landmark # here is where we look for the minimum value

  if (params$time_constraint > 0) {
    floss_constraint <- params$time_constraint
  } else {
    floss_constraint <- floor(history / 2)
  }

  "!DEBUG extract regimes."
  current_abs_min_idx <- 0
  current_abs_min_value <- 1

  all_regimes_idxs <- NULL
  all_regimes_values <- NULL

  # iterates over all cacs of this time series
  purrr::map(floss_list, function(x) {
    cac <- x$cac
    # cac[cac > min_cac] <- 1
    # cac[seq.int(1, history - (4 * 250))] <- 1
    cac[seq.int(1, history - floss_constraint)] <- 1
    min_trigger_idx <- which.min(cac)
    if (min_trigger_idx > landmark) {
      message("min_trigger at ", min_trigger_idx, " for landmark ", landmark, ".")
    }

    if (cac[landmark] < min_cac) {
      abs_min_idx <- x$offset - history + landmark + 1
      if ((abs_min_idx - current_abs_min_idx) > floss_constraint) {
        message("abs_min_idx at ", abs_min_idx, " value ", cac[landmark], ".")
        current_abs_min_idx <<- abs_min_idx
        current_abs_min_value <<- cac[landmark]
        all_regimes_idxs <<- c(all_regimes_idxs, current_abs_min_idx)
        all_regimes_values <<- c(all_regimes_values, current_abs_min_value)
      }
      if (cac[landmark] < current_abs_min_value) {
        if ((abs_min_idx - current_abs_min_idx) < (10 * 250)) {
          message("abs_min_idx2 at ", abs_min_idx, " value ", cac[landmark], ".")
          current_abs_min_idx <<- abs_min_idx
          current_abs_min_value <<- cac[landmark]
          all_regimes_idxs <<- c(all_regimes_idxs, current_abs_min_idx)
          all_regimes_values <<- c(all_regimes_values, current_abs_min_value)
        }
      }
    }

    # if (min_trigger_idx > (history - floss_constraint)) {
    #   min_trigger_abs_idx <- min_trigger_idx + x$offset - history
    #   "!DEBUG triggered at `min_trigger_abs_idx`."
    #   min_trigger_value <- x$cac[min_trigger_idx]

    #   if ((min_trigger_abs_idx - current_abs_min_idx) < floss_constraint) {
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
    regimes <- dplyr::distinct(regimes, idxs, .keep_all = TRUE) %>% dplyr::arrange(idxs)
  } else {
    regimes <- FALSE
  }

  attr(regimes, "info") <- info
  attr(regimes, "params") <- params

  return(regimes)
}
