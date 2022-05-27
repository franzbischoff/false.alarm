
floss_error <- function(data, ...) {
  # cli::cli_alert(c("*" = "floss_error <<- work here"))
  UseMethod("floss_error")
}

floss_error <- yardstick::new_numeric_metric(floss_error, direction = "minimize")

floss_error_micro <- yardstick::metric_tweak("floss_error_micro", floss_error, estimator = "micro")
floss_error_macro <- yardstick::metric_tweak("floss_error_macro", floss_error, estimator = "macro")

floss_error.data.frame <- function(data, truth, estimate, na_rm = TRUE, estimator = "binary", ...) { # nolint
  # cli::cli_alert(c("*" = "floss_error.data.frame <<- work here"))
  # cli::cli_inform(c("*" = "floss_error.data.frame: dots_n {rlang::dots_n(...)}"))
  if (rlang::dots_n(...) > 0) { # 0
    cli::cli_alert(c("*" = "floss_error.data.frame: dots_names {names(rlang::dots_list(..., .preserve_empty = TRUE))}"))
  }

  "!DEBUG evaluating model."
  sizes <- rlang::expr(.sizes)
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

floss_error_vec <- function(truth, estimate, data_size, na_rm = TRUE, estimator = "binary", ...) {
  # cli::cli_alert(c("*" = "floss_error_vec <<- work here"))
  # cli::cli_inform(c("*" = "floss_error_vec: dots_n {rlang::dots_n(...)}"))
  if (rlang::dots_n(...) > 0) {
    cli::cli_alert(c("*" = "floss_error_vec: dots_names {names(rlang::dots_list(..., .preserve_empty = TRUE))}"))
  }

  cli::cli_inform(c("*" = "Evaluating model: <<- This is usually fast."))
  cli::cli_inform(c("*" = "Evaluating model: estimator {estimator}."))
  cli::cli_inform(c("*" = "Evaluating model: number of recordings {length(estimate)}."))

  floss_error_impl <- function(truth, estimate, data_size, ...) {
    res <- score_regimes(truth, estimate, data_size)
    return(res)
  }

  # if (length(data_size) <= 2) {
  #   cli::cli_abort(c("x" = "data_size len: {length(data_size)}"))
  # }

  for (i in seq.int(1, length(estimate))) {
    lt <- length(truth[[i]])
    le <- length(estimate[[i]])
    if (lt > le) {
      estimate[[i]] <- c(estimate[[i]], rep(-1, lt - le))
    } else {
      truth[[i]] <- c(truth[[i]], rep(-1, le - lt))
    }
  }

  checkmate::assert_true(length(truth) == length(estimate))
  checkmate::assert_true(length(truth) == length(data_size))

  if (estimator == "micro") {
    # cli::cli_inform(c("*" = "floss_error_vec <<- micro"))
    res <- purrr::map2_dbl(
      truth, estimate,
      ~ yardstick::metric_vec_template(
        metric_impl = floss_error_impl,
        truth = ..1,
        estimate = ..2,
        na_rm = na_rm,
        cls = "numeric",
        data_size = 1,
        ...
      )
    )

    res <- sum(res)
    div <- purrr::reduce(data_size, sum) + 1
    return(res / div) # micro is the sum of the scores / length(all_data_set)
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
    return(mean(res)) # macro;
    # res
  }
}
