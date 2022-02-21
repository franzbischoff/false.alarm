
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
#' - detect places where the mov_std is non-finite
#' - add a Gaussian noise where the data is "flat" (e.g.: disconnected lead) to avoid correlation with valid data.
#'

validate_data <- function(data, window_size) {
  checkmate::qassert(data, "N+")
  checkmate::qassert(window_size, "X1")

  # TODO: validate_data() may be useful for MPX too, but needs to be checked.

  windowed_size <- length(data) - window_size + 1
  pad_size <- window_size - 1
  is_finite <- is.finite(data)

  # TODO: This may be used for skipping invalid windows
  # valid_windows <- (movsum_ogita_rcpp(is_finite, window_size) == window_size)
  # For streaming purposes it is valid to use zeros for NA/NaN/Inf
  data[!is_finite] <- .Machine$double.eps^0.5

  # NOTE: The validation below causes actually more noise than solves things
  # check data for repetitions 1 and 2 values forward
  # invalid_diff <- diff(data) == 0
  # invalid_diff2 <- diff(data, lag = 2) == 0
  # invalid_diff <- invalid_diff | invalid_diff2
  # data[c(invalid_diff2, FALSE)] <- 0

  data_std <- false.alarm::mov_std(data, window_size)

  is_finite <- is.finite(data_std)
  data_std[!is_finite] <- .Machine$double.eps^0.5

  # This is approximately 0.015. For ECG's, a std smaller than this is probably a
  # disconnected lead or boundary hit, so we add noise to avoid spurious correlations.
  const <- 1 / 64

  invalid_std <- (data_std < const)
  invalid_std <- c(invalid_std, rep(tail(invalid_std, 1), pad_size))

  total <- sum(invalid_std)

  if (total > 0) {
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

    if (length(scale) > 0) {
      data[invalid_std] <- data[invalid_std] + const * scale * rnorm(total)
    }
  }
  return(data)
}
