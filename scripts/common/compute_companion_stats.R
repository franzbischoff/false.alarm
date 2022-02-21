#' Pre-computes the statistics that are used later on the pipeline.
#'
#' Some statistics may be pre-computed once and reused later on the pipeline. This is used only on this
#' setting, since we are simulating a streaming setting but using a batch approach.
#'
#' @param ecg_data a numeric vector. Contains the data that will be used for computing the results.
#' @param params a `list` of parameters. More on details.
#' @param infos a `list` containing the attributes of the imported file and attributes added later
#'              in the pipeline. More on details.
#'
#' @details
#' The `params` for this function currently are:
#'
#' From mapped values (branches):
#' - window_size (integer)
#'
#' Specific values:
#' - n_workers (integer), number of threads to be used on `muinvn` function.
#'
#' The `infos` is made available in case there is a need to access the attributes of the file that
#' contains the signal that is being processed
#'
#' @family process_ts_in_file
#'

compute_companion_stats <- function(ecg_data, params, infos) {
  checkmate::qassert(ecg_data, "N+")
  checkmate::qassert(params, "L+")
  checkmate::qassert(infos, "L+")

  data_len <- length(ecg_data)
  msd <- muinvn(ecg_data, params$window_size, params$n_workers)
  avg <- msd$avg
  sig <- msd$sig
  ddf <- -1 * diff(ecg_data, lag = params$window_size) / 2
  ddg <- (ecg_data[seq.int(params$window_size + 1, data_len)] - avg[seq.int(2, data_len - params$window_size + 1)]) +
    (ecg_data[seq.int(1, data_len - params$window_size)] - avg[seq.int(1, data_len - params$window_size)])

  result <- list(avg = avg, sig = sig, ddf = ddf, ddg = ddg)

  attr(result, "info") <- attr(ecg_data, "info")
  attr(result, "params") <- params

  return(result)
}
