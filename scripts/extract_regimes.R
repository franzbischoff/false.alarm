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
  min_cac <- mean(params$min_cac) - (1 * sd(params$min_cac))
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

  all_regimes <- 0

  # iterates over all cacs of this time series
  regime_changes <- purrr::map(floss_list, function(x) {
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
        current_abs_min_idx <<- abs_min_idx
        current_abs_min_value <<- cac[landmark]
      }
      if (cac[landmark] < current_abs_min_value) {
        if ((abs_min_idx - current_abs_min_idx) < (10 * 250)) {
          current_abs_min_idx <<- abs_min_idx
          current_abs_min_value <<- cac[landmark]
        }
      }
    }

    all_regimes <<- c(all_regimes, current_abs_min_idx)

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

    x$trigger <- list(
      trigger_abs_idx = current_abs_min_idx,
      trigger_value = current_abs_min_value
    )

    x
  })

  all_regimes <- sort(unique(all_regimes))

  info$all_regimes <- all_regimes

  attr(regime_changes, "info") <- info
  attr(regime_changes, "params") <- params

  return(regime_changes)
}
