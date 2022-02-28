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
  mean_std <- false.alarm::movmean_std(ecg_data, params$window_size)
  # mean_of_sd <- mean(mean_std$sd)
  pad <- rep(1, (params$window_size - 1))
  norm_data <- (ecg_data - c(pad, mean_std$avg)) / c(pad, mean_std$sd)

  fix <- !is.finite(norm_data)

  if (any(fix)) {
    norm_data[fix] <- 0
  }

  # norm_data <- ecg_data

  filters <- list()

  align <- "center"

  # # wander
  # filters$wander <- (mean_of_sd < mean_std$sd)
  # hist_diff
  # filters$hist_diff <- zoo::rollapply(norm_data, params$filter_w_size, hist_diff, fill = 0, align = align)
  # zero_cross_rate
  # filters$zero_cross_rate <- zoo::rollapply(norm_data, params$filter_w_size, zero_cross_rate, fill = 0, align = align)
  # activity: give high values on normal signals that wander; >2

  # sqrt(sum(diff(data)^2))

  # filters$complex <- c(rep(0, floor((params$filter_w_size - 1) / 2)), win_complex(norm_data, params$filter_w_size), rep(0, ceiling((params$filter_w_size - 1) / 2)))
  # filters$complextp <- filters$complex > 10
  mins <- zoo::rollapply(norm_data, params$filter_w_size, min, fill = 0, align = align)
  maxs <- zoo::rollapply(norm_data, params$filter_w_size, max, fill = 0, align = align)
  filters$ampl <- maxs - mins
  filters$ampl[1:1000] <- 0
  rawmins <- zoo::rollapply(ecg_data, params$filter_w_size, min, fill = 0, align = align)
  rawmaxs <- zoo::rollapply(ecg_data, params$filter_w_size, max, fill = 0, align = align)
  filters$rawampl <- rawmaxs - rawmins
  filters$rawampl[1:1000] <- 0
  filters$rawnormampl <- filters$rawampl / get_info(ecg_data)$gain

  # filters$activity <- zoo::rollapply(norm_data, params$filter_w_size, activity, fill = 0, align = align)
  # filters$activitytp <- filters$activity > 2

  # filters$test <- filters$activity + filters$complex
  # filters$testtp <- filters$test > 11

  # complexity2 false negatives
  # filters$complexity <- zoo::rollapply(norm_data, params$filter_w_size, complex, fill = 0, align = align)
  # complex values >6 (100 and 200) seems TP
  # mobility: not sensible. >0.6
  # filters$mobility <- zoo::rollapply(norm_data, params$filter_w_size, mobility, fill = 0, align = align)
  # filters$mobility <- filters$mobility > 0.55
  # turning_points
  # filters$turning_points <- zoo::rollapply(norm_data, params$filter_w_size, turning_points, fill = 0, align = align)
  # # mean more than sd
  # filters$mean_more <- (abs(mean_std$avg) >= mean_std$sd)
  # filters$complex_lim <- filters$complex > params$cplx_lim
  # # abs-mean
  # filters$absmean <- abs(mean_std$avg)
  # # std
  # filters$std <- mean_std$sd
  # # std
  # filters$sig <- mean_std$sig
  # # sum
  # filters$sum <- mean_std$sum
  # # sqrsum
  # filters$sqrsum <- mean_std$sqrsum
  # kurtosis: not sensible > 10
  # filters$kurtosis <- zoo::rollapply(norm_data, params$filter_w_size, e1071::kurtosis, fill = 0, align = "left")
  # skewness
  # filters$skewness <- zoo::rollapply(norm_data, params$filter_w_size, e1071::skewness, fill = 0, align = align)

  # filters <- purrr::map(filters, ~ c(rep(0, floor(params$filter_w_size / 2)), .x))

  attr(filters, "info") <- attr(ecg_data, "info")
  attr(filters, "params") <- params

  return(filters)
}
