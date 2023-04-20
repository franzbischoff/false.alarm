# This pre-processing is done for this pipeline, but can be done in on-line settings

# From Contrast Profile TopK code:
# slightly misleading name. This both identifies valid windows and
# perturbs flat data with a bit of noise, scaled appropriately. If it flips
# any nearest neighbors, they were already unreliable matches.


#' Check and validate the data before the use of the contrast profile.
#'
#' This function is currently used for the shapelet candidates with Contrast Profile.
#' More on details.
#'
#' @param data a numeric vector. The data that will be validated.
#' @param window_size an integer. The size of the rolling window that will validate the data.
#'
#' @details
#'
#' This function is derived from the Contrast Profile original code that first apply some
#' "validation" on the data in order to avoid spurious correlations.
#'
#' Some of the checks made:
#' - replace non-finite values with "zero" (For streaming purposes it is valid to use zeros for NA/NaN/Inf)
#' - detect regions with std approx to zero, or with a constant value (e.g.: disconnected lead)
#' - add a Gaussian noise where the data is "flat" (e.g.: disconnected lead) to avoid correlation with valid data.
#'

validate_data <- function(data, window_size) {
  checkmate::qassert(data, "N+")
  checkmate::qassert(window_size, "X1")

  windowed_size <- length(data) - window_size + 1
  pad_size <- window_size - 1
  is_finite <- is.finite(data)
  is_valid_window <- (false.alarm::mov_sum(is_finite, window_size) == window_size)

  # For streaming purposes it is valid to use zeros for NA/NaN/Inf
  data[!is_finite] <- 0

  is_singularity <- vector("logical", length = windowed_size)
  # Now check for sequences of contants
  for (i in seq_len(windowed_size)) {
    if (!is_singularity[i] && all(data[(i + 1):(i + window_size - 1)] == data[i])) {
      is_singularity[i:(i + window_size - 1)] <- TRUE
    }
  }

  is_valid_window[is_singularity] <- FALSE

  data_std <- false.alarm::mov_std(data, window_size)
  # This is approximately 0.0078. For ECG's, a std smaller than this is probably a
  # disconnected lead or boundary hit, so we add noise to avoid spurious correlations.
  const <- 1 / 128
  invalid_std <- (data_std < const)

  is_valid_window[invalid_std] <- FALSE

  is_valid_window <- c(is_valid_window, rep(tail(is_valid_window, 1), pad_size))

  total <- sum(!is_valid_window)

  if (total > 0) {
    # scale is based on data value: x == 0; abs(x) < 1 or abs(x) >= 1
    scale <- purrr::map_dbl(abs(data[!is_valid_window]), function(x) {
      if (x == 0) { # first case
        s <- 1
      } else if (x < 1) { # second case
        # natural log is negative
        p <- abs(1 + log(x))

        if (p < 0.1) { # original code is p == 0; but near to zero is also a problem
          s <- .Machine$double.eps^0.5
        } else {
          s <- 1 / p
        }
      } else { # third case
        s <- 1 + log(x)
      }
      s
    })

    if (length(scale) > 0) {
      data[!is_valid_window] <- data[!is_valid_window] + const * scale * rnorm(total)
    }
  }
  return(data)
}
