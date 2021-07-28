

filter_data <- function(ecg_data, params) {
  window <- params$window

  norm_data <- false.alarm::znorm(ecg_data)
  mean_std <- false.alarm::movmean_std(norm_data, window)
  mean_of_sd <- mean(mean_std$sd)

  filters <- list()

  # # wander
  # filters$wander <- (mean_of_sd < mean_std$sd)
  # # mean more than sd
  # filters$mean_more <- (abs(mean_std$avg) >= mean_std$sd)
  # complex
  filters$complex <- win_complex(norm_data, window, 0)
  filters$complex_lim <- filters$complex > params$cplx_lim
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
  # kurtosis
  filters$kurtosis <- zoo::rollapply(norm_data, window, e1071::kurtosis, align = "left")
  # skewness
  filters$skewness <- zoo::rollapply(norm_data, window, e1071::skewness, align = "left")

  attr(ecg_data, params$attribute) <- filters

  return(ecg_data)
}
