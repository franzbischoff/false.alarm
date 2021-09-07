extract_regimes <- function(floss_list, params) {
  checkmate::qassert(floss_list, "L+")
  checkmate::qassert(params, "L+")

  info <- attr(floss_list, "info")
  pars <- attr(floss_list, "params")

  checkmate::assert_true(
    all(
      pars$window_size == params$window_size,
      pars$time_constraint == params$time_constraint,
      pars$history == params$history
    )
  )

  # TODO: compute the min cac
  min_cac <- mean(params$min_cac) - (1 * sd(params$min_cac))

  if (params$time_constraint > 0) {
    floss_constraint <- params$time_constraint
  } else {
    floss_constraint <- params$history / 2
  }

  "!DEBUG extract regimes."
  current_abs_min_idx <- 0
  current_abs_min_value <- 1
  # iterates over all cacs of this time series
  regime_changes <- purrr::map(floss_list, function(x) {
    # browser()
    cac <- x$cac
    cac[cac > min_cac] <- 1
    cac[seq.int(1, params$history - floss_constraint)] <- 1
    min_trigger_idx <- which.min(cac)

    if (min_trigger_idx > (params$history - floss_constraint)) {
      min_trigger_abs_idx <- min_trigger_idx + x$offset - params$history
      "!DEBUG triggered at `min_trigger_abs_idx`."
      min_trigger_value <- x$cac[min_trigger_idx]

      if ((min_trigger_abs_idx - current_abs_min_idx) < floss_constraint) {
        # updates the current trigger/min
        if (min_trigger_value < current_abs_min_value) {
          current_abs_min_idx <<- min_trigger_abs_idx
          current_abs_min_value <<- min_trigger_value
        }
      } else {
        current_abs_min_idx <<- min_trigger_abs_idx
        current_abs_min_value <<- min_trigger_value
      }
    }

    x$trigger <- list(
      trigger_abs_idx = current_abs_min_idx,
      trigger_value = current_abs_min_value
    )

    x
  })

  attr(regime_changes, "info") <- info
  attr(regime_changes, "params") <- params

  return(regime_changes)
}
