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
    pred <- clean_pred(regime$idxs, params$batch) # this removes the redundant regime changes
    truth <- clean_truth(params$gold_truth, length(data))
    score <- score_regimes(truth, pred, 0) # length(data)
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
  # Probably we are receiving a tibble
  if (is.list(gtruth) && length(gtruth) > 1) {
    if (length(data_size) == 1) {
      data_size <- rep(0, length(gtruth))
    } else {
      checkmate::assert(length(gtruth) == length(data_size))
    }

    # Proceed if same size
    if (length(gtruth) == length(reported)) {
      scores <- purrr::pmap_dbl(list(gtruth, reported, data_size), score_regimes)
    }

    return(scores)
  } else {
    if (is.list(gtruth) && length(gtruth) == 1) {
      gtruth <- gtruth[[1]]
    }
    if (is.list(reported) && length(reported) == 1) {
      reported <- reported[[1]]
    }
  }

  # browser()

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

  # If there are truth not referenced by the reported, we reference them to the nearest reported
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

  if (data_size <= 0) {
    # computes the error during a period of time, not bounded to the data size
    range <- max(gtruth, reported) - min(gtruth, reported) + 1000
    score <- sum(minv) / (min_points * range)
  } else {
    score <- sum(minv) / (min_points * data_size)
  }

  score
}

score_regimes_limit <- function(gtruth, reported, data_size, max_size = 2500) {
  # Probably we are receiving a tibble
  if (is.list(gtruth) && length(gtruth) > 1) {
    if (length(data_size) == 1) {
      data_size <- rep(0, length(gtruth))
    } else {
      checkmate::assert(length(gtruth) == length(data_size))
    }

    # Proceed if same size
    if (length(gtruth) == length(reported)) {
      scores <- purrr::pmap_dbl(list(gtruth, reported, data_size, max_size), score_regimes_limit)
    }

    return(scores)
  } else {
    if (is.list(gtruth) && length(gtruth) == 1) {
      gtruth <- gtruth[[1]]
    }
    if (is.list(reported) && length(reported) == 1) {
      reported <- reported[[1]]
    }
  }

  # browser()

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

  # If there are truth not referenced by the reported, we reference them to the nearest reported
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

  if (anyNA(minv)) {
    rlang::abort("Minv is NA")
  }

  score <- mean(minv) / max_size

  return(score)
}

# Instead of just using the absolute difference, apply a weighting scheme based on how close a predChange is to a goldTruth.
# Calculate the weighted difference: diff += weight * abs(predChange - goldTruth).
# The weight could be a function of the distance, e.g., weight = exp(-distance).
# Normalize by the sum of the weights.
# Using this approach, the score varies too much with the number of predictions or gtruth.
# using -expm1(-distance) gets almost the same as just getting the mean of the distances.
# Also, there must be a limit to consider, so we can reasonably choose a number of seconds * 250hz
# for example: 10 seconds * 250hz = 2500


score_regimes_weighted <- function(gtruth, reported, data_freq = 250, window = 10) {
  # Probably we are receiving a tibble
  if (is.list(gtruth) && length(gtruth) > 1) {
    if (length(data_freq) == 1) {
      data_freq <- rep(250, length(gtruth))
    } else {
      checkmate::assert(length(gtruth) == length(data_size))
    }

    # Proceed if same size
    if (length(gtruth) == length(reported)) {
      scores <- purrr::pmap_dbl(list(gtruth, reported, data_freq, window), score_regimes_weighted)
    }

    return(scores)
  } else {
    if (is.list(gtruth) && length(gtruth) == 1) {
      gtruth <- gtruth[[1]]
    }
    if (is.list(reported) && length(reported) == 1) {
      reported <- reported[[1]]
    }
  }

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

  # If there are truth not referenced by the reported, we reference them to the nearest reported
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

  minv <- minv / (data_freq * 1000) # 250hz * 1000, to avoid exponential errors

  weights <- exp(-minv)
  sum_weights <- sum(weights)

  score <- sum(weights * minv) / (sum_weights)

  if (anyNA(score)) {
    rlang::abort("Score is NA")
  }

  score <- (score * 1000) / window # x1000 to return to seconds
  return(score)
}

# window parameter will be used to compute if the prediction is a true positive or not
# the limit for event detection is 10 seconds, so a prediction flagged after 10 seconds
# from the gold truth is a true positive. For balance, we will use half of this window
# for predictions made before the gold truth.

# alpha: balance between (alpha)existance and (1-alpha)overlap

# gamma: We expect the gamma function to be defined similarly for both RecallT and PrecisionT.
# Intuitively, in both cases, the cardinality factor should be inversely proportional to the
# number of distinct ranges that a given anomaly range overlaps. Thus, we expect gamma() to
# be generally structured as a reciprocal rational function 1/f(x), where f(x) ≥ 1 is a
# single-variable polynomial and x represents the number of distinct overlap ranges.  A
# typical example for gamma() is 1/x.

# delta: flat, back, front, middle (where the overlap is more important). For Precision, flat is the default.

score_regimes_precision <- function(gtruth, reported, data_size, window = 1000, delta_prec = "flat", delta_rec = "back", gamma = "reciprocal", alpha = 0.5, beta = 1) {
  # Probably we are receiving a tibble
  if (is.list(gtruth) && length(gtruth) > 1) {
    if (length(data_size) == 1) {
      data_size <- rep(0, length(gtruth))
    } else {
      checkmate::assert(length(gtruth) == length(data_size))
    }

    future::plan(future.callr::callr)

    # Proceed if same size
    if (length(gtruth) == length(reported)) {
      scores <- furrr::future_pmap(list(gtruth, reported, data_size, window, delta_prec, delta_rec, gamma, alpha, beta), score_regimes_precision)
    }

    # # Proceed if same size
    # if (length(gtruth) == length(reported)) {
    #   scores <- purrr::pmap(list(gtruth, reported, data_size, window), score_regimes_precision)
    # }

    return(scores)
  } else {
    if (is.list(gtruth) && length(gtruth) == 1) {
      gtruth <- gtruth[[1]]
    }
    if (is.list(reported) && length(reported) == 1) {
      reported <- reported[[1]]
    }
  }

  gtruth <- gtruth[gtruth > 1]
  reported <- reported[reported > 1]

  if (length(gtruth) == 0 || length(reported) == 0) {
    return(tibble::tibble(precision = 0, recall = 0, f1 = 0))
  }

  gtruth <- sort(gtruth)
  reported <- sort(reported)

  precision <- score_precision(gtruth, reported, floor(window / 2), delta = delta_prec)
  recall <- score_recall(gtruth, reported, floor(window / 2), delta = delta_rec, gamma = gamma, alpha = alpha)
  fscore <- f_score(precision, recall, beta)

  return(tibble::tibble(precision = precision, recall = recall, fscore = fscore))
  # return(f1)
}


### Recall ###

# Existence
# Size
# Position
# Cardinality

score_recall <- function(truths, predictions, window, delta = "back", gamma = "reciprocal", alpha = 0.5) {
  checkmate::qassert(alpha, "N[0, 1]")

  if (delta == "back") {
    delta <- "front"
  } else if (delta == "front") {
    delta <- "back"
  }

  # sum of all recalls divided by the number of truths
  recall <- purrr::map_dbl(truths, score_recall_t, predictions, window, delta, gamma, alpha)

  if (anyNA(recall)) {
    rlang::abort("Recall is NA")
  }

  # > 1 happens when the window overlaps
  recall <- pmin(recall, 1)
  recall <- sum(recall) / length(truths)
  return(recall)
}

# alpha rewards existence over the others
score_recall_t <- function(truth, predictions, window, delta, gamma, alpha) {
  existence <- 0
  overlap <- 0

  if (alpha > 0) {
    existence <- alpha * score_existence(truth, predictions, window)
  }

  if (alpha < 1) {
    overlap <- (1 - alpha) * overlap_reward(truth, predictions, window, delta, gamma)
  }

  return(existence + overlap)
}

# Existence
score_existence <- function(truth, predictions, window) {
  res <- purrr::some(predictions, dplyr::between, truth - window, truth + window)

  if (isTRUE(res)) {
    return(1)
  } else {
    return(0)
  }
  # for all predictions, if any intersects the truth[i], then it is 1
}

score_precision <- function(truths, predictions, window, delta = "flat") {
  # iterates over all predictions, compute the precision for each one and sum them
  # divided by the total of predicted anomalies

  gamma <- "one"
  precision <- purrr::map_dbl(predictions, overlap_reward, truths, window, delta, gamma)

  if (anyNA(precision)) {
    rlang::abort("Recall is NA")
  }

  # > 1 happens when the window overlaps
  precision <- pmin(precision, 1)
  precision <- sum(precision) / length(predictions)
  return(precision)
}



# omega reward
overlap_reward <- function(anomaly, overlap_set, window, delta, gamma = c("one", "reciprocal")) {
  # using only for recall
  if (gamma == "one") {
    cardinality <- 1
  } else {
    cardinality <- gamma_function(c(anomaly - window, anomaly + window), overlap_set)
  }


  # on precision, anomaly are the predictions, overlap_set are the truths
  omega_reward <- purrr::map_dbl(overlap_set, function(x) {
    omega_function(c(anomaly - window, anomaly + window), c(x - window, x + window), delta)
  })
  omega_reward <- sum(omega_reward)
  reward <- cardinality * omega_reward
  return(reward)

  # when only one pred is linked to truth, the cardinality is the largest (1); otherwise
  # is [0,1], defined by the application.
}

# gamma function
gamma_function <- function(anomaly_range, overlap_set) {
  cardinality <- sum(dplyr::between(overlap_set, anomaly_range[1], anomaly_range[2]))

  if (cardinality == 0) {
    return(1)
  } else {
    return(1 / cardinality)
  }
}

omega_function <- function(anomaly_range, overlap_range, delta = "flat") {
  a <- seq.int(anomaly_range[1], anomaly_range[2])
  b <- seq.int(overlap_range[1], overlap_range[2])

  if (delta == "flat") {
    omega <- sum(a %in% b) / length(a)
  } else if (delta == "back") {
    # omega is greater when the anomaly is at the back
    omega <- sum(which(a %in% b)) / sum(seq_along(a))
  } else if (delta == "front") {
    omega <- sum(which(b %in% a)) / sum(seq_along(a))
  } else if (delta == "middle") {
    half <- ceiling(length(a) / 2)
    overlap <- (a %in% b)
    if (length(overlap) < 2 * half) {
      sequence <- c(seq_len(half), rev(seq_len(half - 1)))
    } else {
      sequence <- c(seq_len(half), rev(seq_len(half)))
    }
    omega <- sum(overlap * sequence) / (sum(sequence))
  } else {
    rlang::abort("Unrecognized delta parameter")
  }

  return(omega)
}


f1_score <- function(precision, recall) {
  2 * precision * recall / (precision + recall + .Machine$double.eps)
}

f_score <- function(precision, recall, beta) {
  (1 + beta^2) * precision * recall / (beta^2 * precision + recall + .Machine$double.eps)
}
