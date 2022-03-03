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

  # norm_data is the same with or without the / gain

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

  filters$complex <- c(rep(NA, floor((params$filter_w_size - 1) / 2)), win_complex(norm_data, params$filter_w_size), rep(NA, ceiling((params$filter_w_size - 1) / 2)))
  filters$complex2 <- c(rep(NA, floor((params$filter_w_size - 1) / 2)), win_complex(ecg_data, params$filter_w_size), rep(NA, ceiling((params$filter_w_size - 1) / 2)))
  filters$complex[1:1000] <- NA
  filters$complex2[1:1000] <- NA


  # filters$complextp <- filters$complex > 10
  filters$activity <- zoo::rollapply(norm_data, params$filter_w_size, activity, fill = NA, align = align)
  filters$activity[1:1000] <- NA
  filters$activity2 <- zoo::rollapply(ecg_data, params$filter_w_size, activity, fill = NA, align = align)
  filters$activity2[1:1000] <- NA
  mins <- zoo::rollapply(norm_data, params$filter_w_size, min, fill = NA, align = align)
  maxs <- zoo::rollapply(norm_data, params$filter_w_size, max, fill = NA, align = align)
  filters$ampl <- maxs - mins
  filters$ampl[1:1000] <- NA
  rawmins <- zoo::rollapply(ecg_data, params$filter_w_size, min, fill = NA, align = align)
  rawmaxs <- zoo::rollapply(ecg_data, params$filter_w_size, max, fill = NA, align = align)
  filters$amplraw <- rawmaxs - rawmins
  filters$amplraw[1:1000] <- NA


  filters$complex <- tsmp:::zero_one_norm(filters$complex)
  filters$complex2 <- tsmp:::zero_one_norm(filters$complex2)
  filters$activity <- tsmp:::zero_one_norm(filters$activity)
  filters$activity2 <- tsmp:::zero_one_norm(filters$activity2)
  filters$ampl <- tsmp:::zero_one_norm(filters$ampl)
  filters$amplraw <- tsmp:::zero_one_norm(filters$amplraw)

  # filters$complex <- filters$complex / 4
  # filters$complex2 <- filters$complex2 / 6000
  # filters$complex3 <- filters$complex3 / 2
  # filters$complex4 <- filters$complex4 * 2000
  # filters$activity <- filters$activity / 2
  # filters$activity2 <- filters$activity2 / (4 * 1e6)
  # filters$activity3 <- filters$activity3 / 1e3
  # filters$ampl <- filters$ampl / 10
  # filters$amplraw <- filters$amplraw / 10000
  # filters$rawnormampl <- filters$rawnormampl / 2.7

  # filters$activity[filters$activity > 1] <- 1 * 1.5
  # filters$activity2[filters$activity2 > 1] <- 1 * 1.5
  # filters$activity3[filters$activity3 > 1] <- 1 * 1.5
  # filters$ampl[filters$ampl > 1] <- 1 * 1.5
  # filters$amplraw[filters$amplraw > 1] <- 1 * 1.5
  # filters$rawnormampl[filters$rawnormampl > 1] <- 1 * 1.5



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
