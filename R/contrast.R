# TODO: documentation

#' Contrast Profile
#'
#' @param distance (`mpx()` only) A string. Currently accepts `euclidean` and `pearson`. Defaults to `euclidean`.
#'
#' @details ## mpx
#' This algorithm was developed apart from the main Matrix Profile branch that relies on Fast Fourier Transform (FFT) at
#' least in one part of the process. This algorithm doesn't use FFT at all and is several times faster. It also relies
#' on Ogita's work for better precision computing mean and standard deviation (part of the process).
#'
#' @seealso `mpxab()` for the forward and reverse join-similarity.
#'
#' @details # This document
#' Last updated on `r Sys.Date()` using R version `r getRversion()`.
#'
#' @export

contrast <- function(negative, positive, window_size, positive_matrix = NULL, exclusion_zone = 0.5, distance = "euclidean", n_workers = 1, progress = FALSE) {
  if (n_workers > 1) {
    p <- RcppParallel::defaultNumThreads()
    n_workers <- min(n_workers, p)
    RcppParallel::setThreadOptions(numThreads = n_workers)
  }

  result <- contrast_profile_rcpp(
    negative,
    positive,
    window_size,
    positive_matrix,
    mp_time_constraint = 0,
    exclusion_zone,
    s_size = 1,
    n_workers = n_workers,
    as.logical(FALSE),
    as.logical(distance == "euclidean"),
    as.logical(progress)
  )

  if (n_workers > 1) {
    RcppParallel::setThreadOptions(numThreads = p)
  }

  return(result)
}
