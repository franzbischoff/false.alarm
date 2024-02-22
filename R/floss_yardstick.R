#' @export
floss_error <- function(data, ...) {
  # cli::cli_alert(c("*" = "floss_error <<- work here"))
  UseMethod("floss_error")
}

floss_error <- yardstick::new_numeric_metric(floss_error, direction = "minimize")

#' @export
floss_error_micro <- yardstick::metric_tweak("floss_error_micro", floss_error, estimator = "micro")

#' @export
floss_error_macro <- yardstick::metric_tweak("floss_error_macro", floss_error, estimator = "macro")

#' @export
floss_error.data.frame <- function(data, truth, estimate, na_rm = TRUE, estimator = "binary", case_weights = NULL, ...) {
  # nolint
  # cli::cli_alert(c("*" = "floss_error.data.frame <<- work here"))
  # cli::cli_inform(c("*" = "floss_error.data.frame: dots_n {rlang::dots_n(...)}"))
  if (rlang::dots_n(...) > 0L) {
    # 0L
    cli::cli_alert(c("*" = "floss_error.data.frame: dots_names {names(rlang::dots_list(..., .preserve_empty = TRUE))}"))
  }

  "!DEBUG evaluating model."
  sizes <- rlang::expr(.sizes) # nolint
  yardstick::metric_summarizer(
    metric_nm = "floss_error",
    metric_fn = floss_error_vec,
    data = data,
    truth = !!rlang::enquo(truth),
    estimate = !!rlang::enquo(estimate),
    na_rm = na_rm,
    estimator = estimator,
    metric_fn_options = list(data_size = rlang::enquo(sizes)) # purrr::map(data$ts, length))
  )
}

clean_pred <- function(data, threshold = 100L, last = TRUE) {
  if (is.list(data)) {
    data <- purrr::map(data, clean_pred, threshold)
    return(data)
  }
  data <- sort(data)
  if (isTRUE(last)) {
    mask <- c(diff(data) > threshold, TRUE)
  } else {
    mask <- c(TRUE, diff(data) > threshold)
  }
  data[mask]
}

clean_truth <- function(truth, data_size = NULL, first = TRUE, last = TRUE) {
  if (!checkmate::test_true(isTRUE(last) && !is.null(data_size))) {
    cli::cli_abort("If `last` is TRUE, `data_size` must not be NULL.")
  }

  if (is.list(truth)) {
    truth <- purrr::map2(truth, data_size, clean_truth, first, last)
    return(truth)
  }

  truth <- sort(truth)
  mask <- c(diff(truth) > 15, TRUE)
  truth <- truth[mask]

  if (isTRUE(first) && (truth[1] <= 10)) {
    if (length(truth) == 1) {
      truth <- 1
    } else {
      truth <- tail(truth, -1)
    }
  }

  if (isTRUE(last) && (tail(truth, 1) >= (data_size - 10))) {
    if (length(truth) == 1) {
      truth <- data_size
    } else {
      truth <- head(truth, -1)
    }
  }

  return(truth)
}

#' @export
floss_error_vec <- function(truth, estimate, data_size, na_rm = TRUE, estimator = "binary", ...) {
  # cli::cli_alert(c("*" = "floss_error_vec <<- work here"))
  # cli::cli_inform(c("*" = "floss_error_vec: dots_n {rlang::dots_n(...)}"))
  if (rlang::dots_n(...) > 0L) {
    cli::cli_alert(c("*" = "floss_error_vec: dots_names {names(rlang::dots_list(..., .preserve_empty = TRUE))}"))
  }

  # cli::cli_inform(c("*" = "Evaluating model: <<- This is usually fast."))
  # cli::cli_inform(c("*" = "Evaluating model: estimator {estimator}."))
  # cli::cli_inform(c("*" = "Evaluating model: number of recordings {length(estimate)}."))

  floss_error_impl <- function(truth, estimate, data_size, ...) {
    # res <- floss_score(truth, estimate, data_size)
    res <- floss_score_pr(truth, estimate, 250, 10, 4)
    res <- 1 - res # invert F-1 score to match original error where lower is better
    return(res)
  }

  # if (length(data_size) <= 2L) {
  #   cli::cli_abort(c("x" = "data_size len: {length(data_size)}"))
  # }
  estimate <- clean_pred(estimate, 200, TRUE)
  truth <- clean_truth(truth, data_size)
  # 100L is the batch size, this removes the redundant regime changes

  for (i in seq.int(1L, length(estimate))) {
    lt <- length(truth[[i]])
    le <- length(estimate[[i]])
    if (lt > le) {
      estimate[[i]] <- c(estimate[[i]], rep(-1L, lt - le))
    } else {
      truth[[i]] <- c(truth[[i]], rep(-1L, le - lt))
    }
  }

  if (estimator == "micro") {
    # current micro method (sum of errors / dataset length)
    # cli::cli_inform(c("*" = "floss_error_vec <<- micro"))
    res <- purrr::map2_dbl(
      truth, estimate,
      ~ yardstick::metric_vec_template(
        metric_impl = floss_error_impl,
        truth = ..1,
        estimate = ..2,
        na_rm = na_rm,
        cls = "numeric",
        data_size = 1L,
        ...
      )
    )

    res <- sum(res)

    div <- purrr::reduce(data_size, sum) + 1L
    return(res / div / 10) # micro is the sum of the scores / length(all_data_set)
  } else if (estimator == "macro_mean") {
    # current macro method (average of the sum of errors / sample range)
    # cli::cli_inform(c("*" = "floss_error_vec <<- macro"))
    res <- purrr::map2_dbl(
      truth, estimate,
      ~ yardstick::metric_vec_template(
        metric_impl = floss_error_impl,
        truth = ..1,
        estimate = ..2,
        na_rm = na_rm,
        cls = "numeric",
        data_size = 0L,
        ...
      )
    )

    return(mean(res))
  } else if (estimator == "macro_median") {
    # cli::cli_inform(c("*" = "floss_error_vec <<- macro"))
    res <- purrr::map2_dbl(
      truth, estimate,
      ~ yardstick::metric_vec_template(
        metric_impl = floss_error_impl,
        truth = ..1,
        estimate = ..2,
        na_rm = na_rm,
        cls = "numeric",
        data_size = 0L,
        ...
      )
    )
    return(median(res)) # macro;
    # res
  } else if (estimator == "macro") {
    # cli::cli_inform(c("*" = "floss_error_vec <<- macro"))
    res <- purrr::pmap_dbl(
      list(truth, estimate, data_size),
      ~ yardstick::metric_vec_template(
        metric_impl = floss_error_impl,
        truth = ..1,
        estimate = ..2,
        na_rm = na_rm,
        cls = "numeric",
        data_size = ..3,
        ...
      )
    )
    return(mean(res)) # macro;
    # res
  } else if (estimator == "macro_fixed") {
    # cli::cli_inform(c("*" = "floss_error_vec <<- macro"))
    res <- purrr::pmap_dbl(
      list(truth, estimate, data_size),
      ~ yardstick::metric_vec_template(
        metric_impl = floss_error_impl,
        truth = ..1,
        estimate = ..2,
        na_rm = na_rm,
        cls = "numeric",
        data_size = floor(..3 / 15000.0),
        ...
      )
    )
    return(mean(res)) # macro;
    # res
  } else if (estimator == "macro_median_fixed") {
    # cli::cli_inform(c("*" = "floss_error_vec <<- macro"))
    res <- purrr::pmap_dbl(
      list(truth, estimate, data_size),
      ~ yardstick::metric_vec_template(
        metric_impl = floss_error_impl,
        truth = ..1,
        estimate = ..2,
        na_rm = na_rm,
        cls = "numeric",
        data_size = floor(..3 / 15000.0),
        ...
      )
    )
    return(median(res)) # macro;
    # res
  } else if (estimator == "fixed") {
    # cli::cli_inform(c("*" = "floss_error_vec <<- macro"))
    res <- purrr::pmap_dbl(
      list(truth, estimate, data_size),
      ~ yardstick::metric_vec_template(
        metric_impl = floss_error_impl,
        truth = ..1,
        estimate = ..2,
        na_rm = na_rm,
        cls = "numeric",
        data_size = floor(..3 / 15000.0),
        ...
      )
    )
    return(list(res)) # macro;
    # res
  } else if (estimator == "range") {
    # cli::cli_inform(c("*" = "floss_error_vec <<- micro"))
    res <- purrr::map2_dbl(
      truth, estimate,
      ~ yardstick::metric_vec_template(
        metric_impl = floss_error_impl,
        truth = ..1,
        estimate = ..2,
        na_rm = na_rm,
        cls = "numeric",
        data_size = 0L,
        ...
      )
    )

    return(list(res))
  } else {
    # cli::cli_inform(c("*" = "floss_error_vec <<- macro"))
    res <- purrr::pmap_dbl(
      list(truth, estimate, data_size),
      ~ yardstick::metric_vec_template(
        metric_impl = floss_error_impl,
        truth = ..1,
        estimate = ..2,
        na_rm = na_rm,
        cls = "numeric",
        data_size = ..3,
        ...
      )
    )
    return(list(res))
  }
}

#' Calculate the F-score for precision and recall
#'
#' This function calculates the F-score, a measure of precision and recall, for a given ground truth and reported values.
#'
#' @param gtruth A numeric vector or list of numeric vectors representing the ground truth values.
#' @param reported A numeric vector or list of numeric vectors representing the reported values.
#' @param freq The frequency of the data points (default is 250).
#' @param window The window size in seconds for matching the reported values with the ground truth values (default is 10).
#' @param beta The beta value for balancing precision and recall (default is 4).
#'
#' @details This is a simpler version of the previous function. The recall delta is computed from freq * window.
#' Delta is then used to compute the overlap window between the ground truth and reported values. Half of delta is used
#' for earlier predictions, and full delta is used for later predictions. If this window contains several
#' reported values, just one is accounted as a true positive. If this window contains no reported values,
#' it is accounted as a false negative. The precision is computed as the number of using the previous tp and fn
#' while the false positives are computed as the number of reported values (all of them) that are not in any window.
#'
#' The parameter `beta` is used to balance precision and recall. The default value is 4, which means that recall is
#' 4 times more important than precision. If `beta` is 1, then precision and recall are equally important.
#' If `beta` is 0.25, then precision is 4 times more important than recall.
#'
#' @return The F-score, a value between 0 and 1, representing the balance between precision and recall.
#'
#' @examples
#' # Calculate the F-score for a single ground truth and reported values
#' score_pr(c(1000, 2000, 3000, 4000), c(2000, 3000, 4000, 5000), window = 1) # 0.75
#'
#' # Calculate the F-score for multiple ground truth and reported values
#' score_pr(list(c(1000, 2000, 3000, 4000), c(5000, 6000, 7000, 8000)),
#'   list(c(2000, 3000, 4000, 5000), c(6000, 7000, 8000, 9000)),
#'   window = 1
#' ) # 0.75 0.75
#'
#' @references
#' For more information on the F-score, see: https://en.wikipedia.org/wiki/F1_score
#'

floss_score_pr <- function(gtruth, reported, freq = 250, window = 10, beta = 4) {
  delta <- freq * window
  gtruth <- sort(gtruth[gtruth > 0L])
  reported <- sort(reported[reported > 0L])
  gtruth <- gtruth[gtruth > 1]
  reported <- reported[reported > 1]

  if (length(gtruth) == 0 || length(reported) == 0) {
    return(0)
  }

  win <- list()
  tp <- 0
  fn <- 0

  fscore <- rlang::try_fetch(
    {
      for (i in seq_len(length(gtruth))) {
        win[[i]] <- seq.int(gtruth[i] - floor(delta / 2), gtruth[i] + floor(delta))
        if (any(reported %in% win[[i]])) {
          tp <- tp + 1
        } else {
          fn <- fn + 1
        }
      }

      win <- unique(unlist(win))
      win <- win[win > 1]

      fp <- sum(!(reported %in% win))

      fscore <- ((1 + beta^2) * tp) / ((1 + beta^2) * tp + beta^2 * fn + fp + .Machine$double.eps)
      fscore
    },
    error = function(cnd) {
      cli::cli_warn("Something wrong.")
      cli::cli_warn("gtruth = {gtruth}.")
      cli::cli_warn("reported = {reported}.")
      cli::cli_warn("minv = {minv}.")
      fscore <- 0
      fscore
    }
  )
  return(fscore)
}

floss_score <- function(gtruth, reported, data_size) {
  gtruth <- sort(gtruth[gtruth > 0L])
  reported <- sort(reported[reported > 0L])

  truth_len <- length(gtruth)
  reported_len <- length(reported)

  if (truth_len == 0) {
    gtruth <- 1
    truth_len <- 1
  }

  if (reported_len == 0) {
    reported <- 1
    reported_len <- 1
  }

  min_points <- min(truth_len, reported_len)

  minv <- rep(Inf, reported_len)

  k <- 1L
  l <- NULL

  score <- rlang::try_fetch(
    {
      for (j in seq.int(1L, reported_len)) {
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
        k <- 1L
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

      if (data_size <= 0L) {
        # computes the mean error during a period of time, not bounded to the data size
        range <- max(gtruth, reported) - min(gtruth, reported)
        score <- sum(minv) / (min_points * range)
      } else {
        score <- sum(minv) / (min_points * data_size)
      }

      score
    },
    error = function(cnd) {
      cli::cli_warn("Something wrong.")
      cli::cli_warn("gtruth = {gtruth}.")
      cli::cli_warn("reported = {reported}.")
      cli::cli_warn("minv = {minv}.")
      score <- 1000
      score
    }
  )

  score
}
