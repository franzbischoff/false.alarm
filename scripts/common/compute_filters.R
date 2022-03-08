#' Pre-computes the filters markers that are used later on the pipeline.
#'
#' @param ecg_data a numeric vector. Contains the data that will be used for computing the results.
#' @param params a `list` of parameters. More on details.
#' @param infos a `list` containing the attributes of the imported file and attributes added later
#'              in the pipeline. More on details.
#'
#' @details
#' The `params` for this function currently are:
#'
#' Specific values:
#' - filter_w_size (integer), the window size to be used by the filter.
#' - cplx_lim (numeric), the cutoff for the complexity filter.
#'
#' The `infos` is made available in case there is a need to access the attributes of the file that
#' contains the signal that is being processed
#'

compute_filters <- function(ecg_data, params, infos) {
  checkmate::qassert(ecg_data, "N+")
  checkmate::qassert(params, "L+")
  checkmate::qassert(infos, "L+")

  # norm_data <- false.alarm::znorm(ecg_data)
  data_size <- length(ecg_data)
  mean_std <- matrixprofiler::movmean_std(ecg_data, params$window_size)
  # mean_of_sd <- mean(mean_std$sd)
  pad <- rep(1, (params$window_size - 1))
  norm_data <- (ecg_data - c(pad, mean_std$avg)) / c(pad, mean_std$sd)

  fix <- !is.finite(norm_data)

  if (any(fix)) {
    norm_data[fix] <- 0
  }

  # norm_data is the same with or without the / gain

  filters <- list()

  align <- "center"
  start_s <- 1:1000
  end_s <- (data_size - 1000 + 1):data_size
  all_s <- c(start_s, end_s)

  # # wander
  # filters$wander <- (mean_of_sd < mean_std$sd)
  # hist_diff
  # filters$hist_diff <- zoo::rollapply(norm_data, params$filter_w_size, hist_diff, fill = 0, align = align)
  # zero_cross_rate
  # filters$zero_cross_rate <- zoo::rollapply(norm_data, params$filter_w_size, zero_cross_rate, fill = 0, align = align)
  # activity: give high values on normal signals that wander; >2

  # sqrt(sum(diff(data)^2))

  filters$complex_norm <- c(rep(NA, floor((params$filter_w_size - 1) / 2)), win_complex(norm_data, params$filter_w_size), rep(NA, ceiling((params$filter_w_size - 1) / 2)))
  filters$complex_raw <- c(rep(NA, floor((params$filter_w_size - 1) / 2)), win_complex(ecg_data, params$filter_w_size), rep(NA, ceiling((params$filter_w_size - 1) / 2)))
  filters$complex_norm[all_s] <- NA
  filters$complex_raw[all_s] <- NA

  # filters$complextp <- filters$complex > 10
  filters$activity_norm <- zoo::rollapply(norm_data, params$filter_w_size, activity, fill = NA, align = align)
  filters$activity_norm[all_s] <- NA
  filters$activity_raw <- zoo::rollapply(ecg_data, params$filter_w_size, activity, fill = NA, align = align)
  filters$activity_raw[all_s] <- NA

  filters$mobility_norm <- zoo::rollapply(norm_data, params$filter_w_size, mobility, fill = NA, align = align)
  filters$mobility_norm[all_s] <- NA
  filters$mobility_raw <- zoo::rollapply(ecg_data, params$filter_w_size, mobility, fill = NA, align = align)
  filters$mobility_raw[all_s] <- NA

  filters$complexity_norm <- zoo::rollapply(norm_data, params$filter_w_size, complexity, fill = NA, align = align)
  filters$complexity_norm[all_s] <- NA
  filters$complexity_raw <- zoo::rollapply(ecg_data, params$filter_w_size, complexity, fill = NA, align = align)
  filters$complexity_raw[all_s] <- NA

  mins <- zoo::rollapply(norm_data, params$filter_w_size, min, fill = NA, align = align)
  maxs <- zoo::rollapply(norm_data, params$filter_w_size, max, fill = NA, align = align)
  filters$ampl_norm <- maxs - mins
  filters$ampl_norm[all_s] <- NA
  rawmins <- zoo::rollapply(ecg_data, params$filter_w_size, min, fill = NA, align = align)
  rawmaxs <- zoo::rollapply(ecg_data, params$filter_w_size, max, fill = NA, align = align)
  filters$ampl_raw <- rawmaxs - rawmins
  filters$ampl_raw[all_s] <- NA

  attr(filters, "info") <- attr(ecg_data, "info")
  attr(filters, "params") <- params

  return(filters)
}
