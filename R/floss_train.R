
#' @export
train_regime_model <- function(truth, ts, ..., window_size, regime_threshold, regime_landmark, mp_threshold, time_constraint) {
  cli::cli_alert(c("!" = "Training the model: <<- this takes time"))
  # cli::cli_inform(c("*" = "train_regime_model: dots_n {rlang::dots_n(...)}"))
  if (rlang::dots_n(...) > 0) { # 0
    cli::cli_alert(c("*" = "train_regime_model: dots_names {names(rlang::dots_list(..., .preserve_empty = TRUE))}"))
  }
  dots <- rlang::dots_list(...)

  n <- nrow(ts)
  if (n == 0) {
    rlang::abort("There are zero rows in the predictor set.")
  }

  cli::cli_inform(c("*" = "Training the model: window {window_size}, threshold {mp_threshold}, time constraint {time_constraint}."))
  cli::cli_inform(c("*" = "Training the model: number of recordings {n}."))

  "!DEBUG fitting model."

  if (ncol(ts) > 1) {
    id <- ts[[1]]
    ts <- ts[[2]]
  } else {
    id <- seq.int(1, n)
  }

  res <- list(truth = truth, id = as.character(id))

  cli::cli_inform(c("*" = "Training the model: using `furrr`."))
  floss <- furrr::future_map(ts,
    training_regimes,
    window_size,
    mp_threshold,
    time_constraint,
    .options = furrr::furrr_options(seed = TRUE, scheduling = 1)
  )

  # cli::cli_inform(c("*" = "Training the model: using `purrr`."))
  # floss <- purrr::map(
  #   ts,
  #   training_regimes,
  #   window_size,
  #   mp_threshold,
  #   time_constraint
  # )

  res$floss <- floss

  rm(floss)
  gc(verbose = FALSE)

  trained <- list(
    fitted.values = tibble::as_tibble(res),
    terms = list(
      window_size = window_size,
      mp_threshold = mp_threshold,
      time_constraint = time_constraint,
      regime_threshold = regime_threshold,
      regime_landmark = regime_landmark
    )
  )

  class(trained) <- "floss_regime_model"

  return(trained)
}
