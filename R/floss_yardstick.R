
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

clean_pred <- function(data, threshold = 100L) {
  if (is.list(data)) {
    data <- purrr::map(data, clean_pred, threshold)
    return(data)
  }

  data <- sort(data)
  mask <- c(diff(data) > threshold, TRUE)
  data[mask]
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
    res <- floss_score(truth, estimate, data_size)
    return(res)
  }

  # if (length(data_size) <= 2L) {
  #   cli::cli_abort(c("x" = "data_size len: {length(data_size)}"))
  # }
  estimate <- clean_pred(estimate)
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
