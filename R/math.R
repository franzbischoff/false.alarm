# Math functions ------------------------------------------------------------------------------
#
# Supports:
#               NA/NaN  -Inf/Inf
# std             Unk      Unk
# mode            Unk      Unk
# znorm           Unk      Unk

#' Math Functions
#'
#' `znorm()`: Normalizes data for mean Zero and Standard Deviation One
#'
#' @return `znorm()`: Returns the normalized data
#' @export
#' @order 1
#' @rdname math_tools
#' @examples
#' normalized <- znorm(tsmp::motifs_discords_small)
znorm <- function(data, rcpp = TRUE) {
  # Rcpp is faster
  if (rcpp) {
    return(znorm_rcpp(data)) # nolint
  }

  data_mean <- mean(data)
  data_dev <- std(data)

  if (is.na(data_dev) || data_dev <= 0.01) {
    return(data - data_mean)
  } else {
    (data - data_mean) / (data_dev)
  }
}

#' Math Functions
#'
#' `ed_corr()`: Converts euclidean distances into correlation values
#'
#' @param data a `vector` of `numeric`.
#' @param w the window size
#' @param rcpp A `logical`. If `TRUE` will use the Rcpp implementation, otherwise will use the R implementation,
#' that may or not be slower.
#'
#' @return `ed_corr()`: Returns the converted values from euclidean distance to correlation values.
#' @export
#' @order 2
#' @rdname math_tools
#' @examples
#' fake_data <- c(rep(3, 100), rep(2, 100), rep(1, 100))
#' correlation <- ed_corr(fake_data, 50)
ed_corr <- function(data, w, rcpp = TRUE) {
  if (rcpp) {
    return(ed_corr_rcpp(data, w)) # nolint
  } else {
    return((2.0 * w - data^2.0) / (2.0 * w))
  }
}

#' Math Functions
#'
#' `corr_ed()`: Converts correlation values into euclidean distances
#'
#' @return `corr_ed()`: Returns the converted values from euclidean distance to correlation values.
#' @export
#' @order 3
#' @rdname math_tools
#' @examples
#' fake_data <- c(rep(0.5, 100), rep(1, 100), rep(0.1, 100))
#' euclidean <- corr_ed(fake_data, 50)
corr_ed <- function(data, w, rcpp = TRUE) {
  if (rcpp) {
    return(corr_ed_rcpp(data, w)) # nolint
  } else {
    sqrt(2.0 * w * (1.0 - pmin(data, 1.0)))
  }
}

#' Math Functions
#'
#' `mode()`: Returns the most common value from a vector of integers
#'
#' @param x a `vector` of `integers`.
#'
#' @return `mode()`: Returns the most common value from a vector of integers.
#' @export
#' @order 4
#' @rdname math_tools
#' @examples
#' fake_data <- c(1, 1, 4, 5, 2, 3, 1, 7, 9, 4, 5, 2, 3)
#' mode <- mode(fake_data)
mode <- function(x, rcpp = FALSE) {
  x <- as.integer(x)

  # Rcpp is not faster
  if (rcpp) {
    return(mode_rcpp(x)) # nolint
  }

  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' Math Functions
#'
#' `std()`: Population SD, as R always calculate with n-1 (sample), here we fix it.
#'
#' @param na.rm A logical. If `TRUE` remove the `NA` values from the computation.
#' @return `std()`: Returns the corrected standard deviation from sample to population.
#' @export
#' @order 5
#' @rdname math_tools
#' @examples
#' fake_data <- c(1, 1.4, 4.3, 5.1, 2, 3.6, 1.24, 2, 9, 4.3, 5, 2.1, 3)
#' res <- std(fake_data)
std <- function(data, na.rm = FALSE, rcpp = TRUE) {
  # Rcpp is faster
  if (rcpp) {
    return(std_rcpp(data, na.rm)) # nolint
  }

  sdx <- stats::sd(data, na.rm)

  if (is.na(sdx)) {
    return(NA)
  }

  return(sqrt((length(data) - 1L) / length(data)) * sdx)
}

#' Math Functions
#'
#' `normalize()`: Normalizes data to be between min and max.
#'
#' @param min_lim A number
#' @param max_lim A number
#' @return `normalize()`: Returns the normalized data between min and max.
#' @export
#' @order 6
#' @rdname math_tools
#' @examples
#' fake_data <- c(1, 1.4, 4.3, 5.1, 2, 3.6, 1.24, 1, 9, 4.3, 5, 2.1, 3)
#' res <- normalize(fake_data)
normalize <- function(data, min_lim = 0.0, max_lim = 1.0, rcpp = FALSE) {
  if (rcpp) {
    na <- sort(which(is.na(data)))
    data <- data[!is.na(data)]
    data <- normalize_rcpp(data, min_lim, max_lim) # nolint
    for (n in na) {
      data <- append(data, NA, n - 1.0)
    }
    return(data)
  }

  min_val <- min(data, na.rm = TRUE)
  max_val <- max(data, na.rm = TRUE)

  a <- (max_lim - min_lim) / (max_val - min_val)
  b <- max_lim - a * max_val
  data <- a * data + b

  data[data < min_lim] <- min_lim
  data[data > max_lim] <- max_lim

  return(data)
}

#' Math Functions
#'
#' `complexity()`: Computes the complexity index of the data
#'
#' @return `complexity()`: Returns the complexity index of the data provided (normally a subset).
#' @export
#' @order 7
#' @rdname math_tools
#' @examples
#' fake_data <- c(1, 1.4, 4.3, 5.1, 2, 3.6, 1.24, 8, 9, 4.3, 5, 2.1, 3)
#' res <- complexity(fake_data)
complexity <- function(data) {
  return(sqrt(sum(diff(data)^2.0)))
}

#' Math Functions
#'
#' `binary_split()`: Creates a vector with the indexes of binary split.
#'
#' @param n size of the vector
#'
#' @return `complexity()`: Returns a `vector` with the binary split indexes.
#' @export
#' @order 8
#' @rdname math_tools
#' @examples
#' fake_data <- c(10)
#' res <- binary_split(fake_data)
binary_split <- function(n, rcpp = TRUE) {
  if (rcpp) {
    res <- binary_split_rcpp(as.integer(n)) # nolint
    return(as.integer(res))
  }

  if (n < 2L) {
    return(1L)
  }

  split <- function(lb, ub, m) {
    if (lb == m) {
      l <- NULL
      r <- c(m + 1L, ub)
    } else if (ub == m) {
      l <- c(lb, m - 1L)
      r <- NULL
    } else {
      l <- c(lb, m - 1L)
      r <- c(m + 1L, ub)
    }

    return(list(l = l, r = r))
  }

  idxs <- vector(mode = "numeric", length = n)
  intervals <- list()

  idxs[1L] <- 1L # We always begin by explore the first integer
  intervals[[1L]] <- c(2L, n) # After exploring the first integer, we begin splitting the interval 2L:n
  i <- 2L

  while (length(intervals) > 0L) {
    lb <- intervals[[1L]][1L]
    ub <- intervals[[1L]][2L]
    mid <- floor((lb + ub) / 2.0)
    intervals[[1L]] <- NULL

    idxs[i] <- mid
    i <- i + 1L

    if (lb == ub) {
      next
    } else {
      lr <- split(lb, ub, mid)
      if (!is.null(lr$l)) {
        intervals[[length(intervals) + 1L]] <- lr$l
      }
      if (!is.null(lr$r)) {
        intervals[[length(intervals) + 1L]] <- lr$r
      }
    }
  }
  return(as.integer(idxs))
}


#' Math Functions
#'
#' `motif_quality()`: Computes a coefficient of quality for the best motif in the matrix profile. Depends on
#'  the window size used for the matrix profile computation.
#'
#' @param mp a `vector` of `numeric`. The computed matrix profile.
#' @param method a `string`. What will be the comparative measurement. Currently "mean" or "median". Default is "mean".
#' @param input_format a `string`. The values in `mp` are "euclidean" distances or "pearson" coefficients? Default is "euclidean".
#' @param window_size an `integer` or `NULL`. If `input_format` is "pearson", this value must be given. Default is `NULL`.
#'
#' @return `motif_quality()`: Returns a value between 0.0 and 1.0. Large is better.
#' @export
#' @order 9
#' @rdname math_tools
#' @examples
#' fake_data <- c(1, 1, 4, 5, 2, 3, 1, 7, 9, 4, 5, 2, 3)
#' coef <- motif_quality(fake_data)
motif_quality <- function(mp, method = c("mean", "median"), input_format = c("euclidean", "pearson"), window_size = NULL) {
  method <- match.arg(method)
  input_format <- match.arg(input_format)

  if (input_format == "pearson") {
    if (!is.numeric(window_size)) {
      stop("For input format 'pearson', the window_size must be given.", call. = FALSE)
    }
    mp <- corr_ed(mp, window_size)
  }

  result <- 1.0 - (min(mp) / do.call(method, list(mp)))

  return(result)
}
