compute_companion_stats <- function(ecg_data, params) {

  checkmate::qassert(ecg_data, "N+")

  data_len <- length(ecg_data)
  msd <- muinvn(ecg_data, params$window_size, params$n_workers)
  avg <- msd$avg
  sig <- msd$sig
  ddf <- diff(ecg_data, lag = params$window_size) / 2
  ddg <- (ecg_data[seq.int(params$window_size + 1, data_len)] - avg[seq.int(2, data_len - params$window_size + 1)]) + (ecg_data[seq.int(1, data_len - params$window_size)] - avg[seq.int(1, data_len - params$window_size)])

  result <- list(avg = avg, sig = sig, ddf = ddf, ddg = ddg)

  attr(result, "info") <- attr(ecg_data, "info")

  return(result)
}
