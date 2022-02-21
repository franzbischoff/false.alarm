#' Extract the changepoints of detected regimes.
#'
#' This function is used inside the pipeline. It receives several parameters for establishing the changepoints.
#' More on details.
#'
#' @param floss_list a `list` of each computed 'CAC' (Corrected Arc Counts) in one signal.
#' @param params a `list` of parameters. More on details.
#' @param infos a `list` containing the attributes of the imported file and attributes added later
#'              in the pipeline. More on details.
#'
#' @details
#' This function simulates the streaming scenario by jumping from each element on the `floss_list` that was created
#' in batches. This allows us to have small snapshots of the ongoing process for inspection, debugging, or stop/resume/update
#' the pipeline without having to re-simulate the full streaming.
#'
#' Some parameters may look redundant but serves as assertation of the pipeline, assuring that the
#' branches are not being mixed
#'
#' The `params` for this function currently are:
#'
#' From mapped values (branches):
#' - window_size (integer)
#' - regime_threshold (numeric)
#' - mp_time_constraint (integer)
#' - floss_time_constraint (integer)
#'
#' Common values:
#' - regime_landmark (integer), currently 3 seconds from the end.
#' - history (integer), the size of the history buffer
#'
#' The `infos` is made available in case there is a need to access the attributes of the file that
#' contains the signal that is being processed
#'
#' @family process_ts_in_file


extract_regimes <- function(floss_list, params, infos) {
  checkmate::qassert(floss_list, "L+")
  checkmate::qassert(params, "L+")

  info <- attr(floss_list, "info")
  pars <- attr(floss_list, "params")
  # subset_start <- ifelse(isFALSE(info$subset), 0, info$subset[1] - 1)

  checkmate::assert_true(
    all(
      pars$window_size == params$window_size,
      pars$mp_time_constraint == params$mp_time_constraint,
      pars$history == params$history
    )
  )

  regime_threshold <- params$regime_threshold
  window_size <- params$window_size
  history <- params$history
  landmark <- history - params$regime_landmark # here is where we look for the minimum value

  if (params$mp_time_constraint > 0) {
    floss_constraint <- params$mp_time_constraint
  } else {
    floss_constraint <- floor(history / 2)
  }

  "!DEBUG extract regimes."
  current_abs_min_idx <- 0
  current_abs_min_value <- 1

  all_regimes_idxs <- NULL
  all_regimes_values <- NULL

  # TODO: Review if landmark corresponds in cac the index of data

  # iterates over all cacs of this time series
  purrr::map(floss_list, function(x) {
    cac <- x$cac
    # cac[cac > regime_threshold] <- 1
    # cac[seq.int(1, history - (4 * 250))] <- 1
    cac[seq.int(1, history - floss_constraint)] <- 1
    min_trigger_idx <- which.min(cac)
    # if (min_trigger_idx > landmark) {
    #   message("min_trigger at ", min_trigger_idx, " for landmark ", landmark, ".")
    # }

    if (cac[landmark] < regime_threshold) {
      abs_min_idx <- x$offset - history + landmark + 1
      if ((abs_min_idx - current_abs_min_idx) > floss_constraint) { # IMPROVE: tweak floss_constraint
        message("abs_min_idx at ", abs_min_idx, " value ", cac[landmark], ".")
        current_abs_min_idx <<- abs_min_idx
        current_abs_min_value <<- cac[landmark]
        all_regimes_idxs <<- c(all_regimes_idxs, current_abs_min_idx)
        all_regimes_values <<- c(all_regimes_values, current_abs_min_value)
      }
      if (cac[landmark] < current_abs_min_value) {
        if ((abs_min_idx - current_abs_min_idx) < floor(history / 2)) { # IMPROVE: tweak floor(history / 2)
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
    regimes <- dplyr::distinct(regimes, idxs, .keep_all = TRUE) %>% dplyr::arrange(idxs) # nolint
  } else {
    regimes <- FALSE
  }

  attr(regimes, "info") <- info
  attr(regimes, "params") <- params

  return(regimes)
}
