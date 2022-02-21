#' Title
#'
#' Short description
#'
#' @param params a `list` of parameters. More on details.
#' @param infos a `list` containing the attributes of the imported file and attributes added later
#'              in the pipeline. More on details.
#'
#' @details
#' Details
#'
#' @family process_ts_in_file
#'


extract_regime_sample <- function(data_with_regimes, params, infos) {
  checkmate::qassert(data_with_regimes, "L2")
  checkmate::qassert(params, "L+")
  checkmate::qassert(infos, "L+")

  checkmate::assert_true(identical(
    attr(data_with_regimes[[1]], "info"),
    attr(data_with_regimes[[2]], "info")
  ))

  data <- data_with_regimes[[1]]
  data_info <- attr(data, "info")
  subset_start <- ifelse(isFALSE(data_info$subset), 0, data_info$subset[1] - 1)
  regimes <- data_with_regimes[[2]]

  if (isFALSE(regimes)) {
    return(FALSE)
  }

  pars <- attr(regimes, "params")

  # don't compute if the stats are not compatible
  checkmate::assert_true(
    all(
      pars$window_size == params$window_size,
      pars$mp_time_constraint == params$mp_time_constraint,
      pars$history == params$history
    )
  )

  regime_threshold <- params$regime_threshold
  landmark <- params$regime_landmark # here is where we look for the minimum value

  samples <- list()

  for (i in seq_len(nrow(regimes))) {
    idx <- regimes$idxs[i]
    value <- regimes$values[i]
    start <- idx - subset_start
    end <- start + landmark # IMPROVE: May be another value
    samples[[as.character(idx)]] <- data[seq.int(start, end)]
    attr(samples[[as.character(idx)]], "info") <- list(idx = idx, value = value)
  }

  attr(samples, "info") <- data_info
  attr(samples, "params") <- params

  return(samples)
}
