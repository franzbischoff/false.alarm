#' Compute the Matrix Profile using batches, using the pre-computed statistics, for speed
#'
#' @param data_regimes a `list` of size 2. Contains the data that will be used for computing the
#'   results and the pre-computed statistics.
#' @param params a `list` of parameters. More on details.
#' @param infos a `list` containing the attributes of the imported file and attributes added later
#'              in the pipeline. More on details.
#'
#' @details
#' The `params` for this function currently are:
#'
#' From mapped values (branches):
#' - window_size (integer)
#' - threshold (numeric), similarity threshold
#' - mp_time_constraint (integer)
#'
#' Common values:
#' - ez (numeric), the exclusion zone to be used on MPX (proportion)
#' - batch (integer), the size of the batch for simulate the stream.
#' - history (integer), the size of the history buffer
#'
#' Specific values:
#' - progress (logical), TRUE for printing MPX progress.
#'
#' The `infos` is made available in case there is a need to access the attributes of the file that
#' contains the signal that is being processed
#'
#' @family process_ts_in_file
#'

compute_score_regimes <- function(data_regimes, params, infos) {
  checkmate::qassert(data_regimes, "L2")
  checkmate::assert_true(identical(
    attr(data_regimes[[1]], "info"),
    attr(data_regimes[[2]], "info")
  ))
  checkmate::qassert(infos, "L+")

  data <- data_regimes[[1]]
  data_info <- attr(data, "info")
  regime <- data_regimes[[2]]

  # don't compute if the regime are not compatible
  checkmate::assert_true(
    attr(regime, "params")$window_size == params$window_size
  )

  "!DEBUG Score regime truth `params$gold_truth`, predicted `regime$idxs`"

  if (!isFALSE(regime)) {
    idxs <- sort(regime$idxs)
    idxs <- idxs[diff(idxs) > params$batch] # this removes the redundant regime changes
    score <- score_regimes(params$gold_truth, regime$idxs, length(data))
    "!DEBUG Finished Score."
    regime$score <- score
  } else {
    "!DEBUG Regime is empty."
  }

  attr(regime, "info") <- data_info
  attr(regime, "params") <- params

  return(regime)
}


score_regimes <- function(gtruth, reported, data_size) {
  gtruth <- sort(gtruth[gtruth > 0])
  reported <- sort(reported[reported > 0])

  truth_len <- length(gtruth)
  reported_len <- length(reported)

  min_points <- min(truth_len, reported_len)

  minv <- rep(Inf, reported_len)

  k <- 1
  l <- NULL

  for (j in seq.int(1, reported_len)) {
    for (i in seq.int(k, truth_len)) {
      if (abs(gtruth[i] - reported[j]) <= minv[j]) {
        minv[j] <- abs(gtruth[i] - reported[j])
        k <- i # pruning, truth and reported must be sorted
      } else {
        l <- c(l, k)
        break # pruning, truth and reported must be sorted
      }
    }
  }

  if (truth_len > reported_len) {
    lefties <- seq_len(truth_len)
    lefties <- lefties[!(lefties %in% l)]
    minv_left <- rep(Inf, truth_len)
    k <- 1
    for (j in lefties) {
      for (i in seq.int(k, reported_len)) {
        if (abs(gtruth[j] - reported[i]) <= minv_left[j]) {
          minv_left[j] <- abs(gtruth[j] - reported[i])
          k <- i # pruning, truth and reported must be sorted
        } else {
          break # pruning, truth and reported must be sorted
        }
      }
    }

    minv_left <- minv_left[is.finite(minv_left)]
    minv <- c(minv, minv_left)
  }

  score <- sum(minv) / (min_points * data_size)
  score
}


score_regimes_old <- function(gtruth, reported, data_size) {

  # FIXME: Fix this on regime_test
  # idxs <- sort(regime$idxs)
  # idxs <- idxs[diff(idxs) > params$batch] # this removes the redundant regime changes
  # score <- score_regimes(params$gold_truth, regime$idxs, length(data))
  # "!DEBUG Finished Score."
  # regime$score <- score


  gtruth <- gtruth[gtruth > 0]
  reported <- reported[reported > 0]

  m <- length(gtruth)
  n <- length(reported)

  out <- max(m, n)
  inn <- min(m, n)

  # FIXME: by default, outer should be the reported (paper!); swaping this gives different results.
  if (out == m) {
    outer <- sort(gtruth)
    inner <- sort(reported)
  } else {
    outer <- sort(reported)
    inner <- sort(gtruth)
  }

  # minv <- rep(Inf, out)

  # for (j in seq.int(1, out)) {
  #   for (i in seq.int(1, inn)) {
  #     if (abs(inner[i] - outer[j]) < abs(minv[j])) {
  #       minv[j] <- abs(inner[i] - outer[j])
  #     }
  #   }
  # }

  minv <- rep(Inf, out)

  k <- 1

  for (j in seq.int(1, out)) {
    for (i in seq.int(k, inn)) {
      if (abs(inner[i] - outer[j]) <= minv[j]) {
        minv[j] <- abs(inner[i] - outer[j])
        k <- i # pruning, truth and reported must be sorted
      } else {
        break # pruning, truth and reported must be sorted
      }
    }
  }

  score <- sum(minv) / (inn * data_size)
}
