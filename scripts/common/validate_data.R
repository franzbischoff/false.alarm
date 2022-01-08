
# This pre-processing is done for this pipeline, but can be done in on-line settings

# From Contrast Profile TopK code:
# slightly misleading name. This both identifies valid windows and
# perturbs flat data with a bit of noise, scaled appropriately. If it flips
# any nearest neighbors, they were already unreliable matches.

validate_data <- function(data, window_size) {
  windowed_size <- length(data) - window_size + 1
  pad_size <- window_size - 1
  is_finite <- is.finite(data)

  # TODO: This may be used for skipping invalid windows
  # valid_windows <- (movsum_ogita_rcpp(is_finite, window_size) == window_size)
  # For streaming purposes it is valid to use zeros for NA/NaN/Inf
  data[!is_finite] <- 0

  # check data for repetitions 1 and 2 values forward
  invalid_diff <- diff(data) == 0
  invalid_diff2 <- diff(data, lag = 2) == 0
  invalid_diff <- invalid_diff | invalid_diff2
  data[c(invalid_diff, FALSE)] <- 0

  data_std <- mov_std(data, window_size)

  is_finite <- is.finite(data_std)
  data_std[!is_finite] <- 0

  # This is approximately 0.015. For ECG's, a std smaller than this is probably a
  # disconnected lead or boundary hit, so we add noise to avoid spurious correlations.
  const <- 1 / 64

  invalid_std <- (data_std < const)

  if (isTRUE(tail(invalid_std, 1))) {
    invalid_std <- c(invalid_std, rep(TRUE, pad_size))
  } else {
    invalid_std <- c(invalid_std, rep(FALSE, pad_size))
  }

  total <- sum(invalid_std)
  # scale is based on data value: x == 0; abs(x) < 1 or abs(x) >= 1
  scale <- purrr::map_dbl(abs(data[invalid_std]), function(x) {
    if (x == 0) { # first case
      s <- 1
    } else if (x < 1) { # second case
      # natural log is negative
      p <- 1 + log(x)

      if (p == 0) {
        s <- .Machine$double.eps^0.5
      } else {
        s <- 1 / abs(p)
      }
    } else { # third case
      s <- 1 + log(x)
    }
    s
  })
  data[invalid_std] <- data[invalid_std] + const * scale * rnorm(total)

  return(data)
}
