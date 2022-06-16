#' floss_regime_model
#'
#' @description
#'
#' floss_regime_model
#'
#' @export
floss_regime_model <- function(mode = "regression",
                               window_size = NULL, time_constraint = NULL,
                               mp_threshold = NULL, regime_threshold = NULL,
                               regime_landmark = NULL, engine = "floss") {
  # Check for correct mode
  if (mode != "regression") {
    rlang::abort("`mode` should be 'regression'")
  }

  "!DEBUG registering model."

  # Capture the arguments in quosures
  args <- list(
    window_size = rlang::enquo(window_size),
    time_constraint = rlang::enquo(time_constraint),
    mp_threshold = rlang::enquo(mp_threshold),
    regime_threshold = rlang::enquo(regime_threshold),
    regime_landmark = rlang::enquo(regime_landmark)
  )

  # Save some empty slots for future parts of the specification
  parsnip::new_model_spec(
    "floss_regime_model",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = engine
  )
}

#' @export
print.floss_regime_model <- function(x, ...) { # nolint
  cat("FLOSS Regime Model Specification (", x$mode, ")\n\n", sep = "")
  parsnip::model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(parsnip::show_call(x))
  }

  invisible(x)
}


#' @export
update.floss_regime_model <- function(object,
                                      parameters = NULL,
                                      window_size = NULL,
                                      time_constraint = NULL,
                                      mp_threshold = NULL,
                                      regime_threshold = NULL,
                                      regime_landmark = NULL,
                                      fresh = FALSE, ...) { # nolint
  # cli::cli_inform(c("*" = "update.floss_regime_model"))
  # cli::cli_inform(c("*" = "update.floss_regime_model: dots_n {rlang::dots_n(...)}"))
  if (rlang::dots_n(...) > 0) {
    cli::cli_alert(c("*" = "update.floss_regime_model: dots_names {names(rlang::dots_list(..., .preserve_empty = TRUE))}"))
  }

  args <- list(
    window_size = rlang::enquo(window_size),
    time_constraint = rlang::enquo(time_constraint),
    mp_threshold = rlang::enquo(mp_threshold),
    regime_threshold = rlang::enquo(regime_threshold),
    regime_landmark = rlang::enquo(regime_landmark)
  )

  # function currently not exported by parsnip
  update_spec <- function(object, parameters, args_enquo_list, fresh, cls, ...) {
    eng_args <- parsnip::update_engine_parameters(object$eng_args, fresh, ...)

    if (!is.null(parameters)) {
      parameters <- parsnip::check_final_param(parameters)
    }

    args <- parsnip::update_main_parameters(args_enquo_list, parameters)

    if (fresh) {
      object$args <- args
      object$eng_args <- eng_args
    } else {
      null_args <- purrr::map_lgl(args, parsnip::null_value)
      if (any(null_args)) {
        args <- args[!null_args]
      }
      if (length(args) > 0) {
        object$args[names(args)] <- args
      }
      if (length(eng_args) > 0) {
        object$eng_args[names(eng_args)] <- eng_args
      }
    }

    parsnip::new_model_spec(
      cls,
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }

  update_spec(
    object = object,
    parameters = parameters,
    args_enquo_list = args,
    fresh = fresh,
    cls = "floss_regime_model",
    ...
  )
}


#' @export
translate.floss_regime_model <- function(x, engine = x$engine, ...) { # nolint
  # cli::cli_inform(c("*" = "translate.floss_regime_model"))
  # cli::cli_inform(c("*" = "translate.floss_regime_model: dots_n {rlang::dots_n(...)}"))
  if (rlang::dots_n(...) > 0) { # 0
    cli::cli_alert(c("*" = "translate.floss_regime_model: dots_names {names(rlang::dots_list(..., .preserve_empty = TRUE))}"))
  }

  if (is.null(engine)) {
    message("Used `engine = 'floss'` for translation.")
    engine <- "floss"
  }

  x <- parsnip::translate.default(x, engine, ...)

  if (engine == "floss") {
    .check_floss_regime_threshold_fit(x)
  }
  x
}

check_args.floss_regime_model <- function(object) { # nolint

  "!DEBUG This runs in fit()"
  # cli::cli_inform(c("*" = "check_args.floss_regime_model"))

  args <- lapply(object$args, rlang::eval_tidy)

  if (length(args$window_size) > 1) {
    rlang::abort("`window_size` must be a single value")
  }
  if (length(args$mp_threshold) > 1) {
    rlang::abort("`mp_threshold` must be a single value")
  }
  if (length(args$time_constraint) > 1) {
    rlang::abort("`time_constraint` must be a single value")
  }
  if (length(args$regime_threshold) > 1) {
    rlang::abort("`regime_threshold` must be a single value")
  }
  if (length(args$regime_landmark) > 1) {
    rlang::abort("`regime_landmark` must be a single value")
  }

  # if (all(is.numeric(args$regime_threshold)) && any(args$regime_threshold < 0 | args$regime_threshold > 1)) {
  #   rlang::abort("The regime_threshold should be within [0,1].")
  # }
  # if (all(is.numeric(args$window_size)) && any(args$window_size <= 0)) {
  #   rlang::abort("The window_size should be a positive integer value.")
  # }
  # if (all(is.numeric(args$mp_threshold)) && any(args$mp_threshold < 0 | args$mp_threshold > 1)) {
  #   rlang::abort("The mp_threshold should be within [0,1].")
  # }
  # if (all(is.numeric(args$time_constraint)) && any(args$time_constraint <= 0)) {
  #   rlang::abort("The time_constraint should be a positive integer value.")
  # }

  invisible(object)
}


#' Helper functions for checking the regime_threshold of FLOSS models
#'
#' @description
#' These functions are for developer use.
#'
#' `.check_floss_regime_threshold_fit()` checks that the model specification for fitting a
#' floss model contains a single value.
#'
#' `.check_floss_regime_threshold_predict()` checks that the regime_threshold value used for prediction is valid.
#' If called by `predict()`, it needs to be a single value. Multiple values are
#' allowed for `multi_predict()`.
#'
#' @param x An object of class `model_spec`.
#' @rdname floss_helpers
#' @keywords internal
#' @export
.check_floss_regime_threshold_fit <- function(x) { # nolint
  # cli::cli_inform(c("*" = ".check_floss_regime_threshold_fit"))
  regime_threshold <- rlang::eval_tidy(x$args$regime_threshold)
  regime_landmark <- rlang::eval_tidy(x$args$regime_landmark)

  if (length(regime_threshold) != 1) {
    rlang::abort(c(
      "For the floss engine, `regime_threshold` must be a single number (or a value of `tune()`).",
      glue::glue("There are {length(regime_threshold)} values for `regime_threshold`."),
      "To try multiple values for total regularization, use the tune package.",
      "To predict multiple regime_threshold, use `multi_predict()`"
    ))
  }

  if (length(regime_landmark) != 1) {
    rlang::abort(c(
      "For the floss engine, `regime_landmark` must be a single number (or a value of `tune()`).",
      glue::glue("There are {length(regime_landmark)} values for `regime_landmark`."),
      "To try multiple values for total regularization, use the tune package.",
      "To predict multiple regime_landmark, use `multi_predict()`"
    ))
  }
}


#' @export
predict.floss_regime_model <- function(object, new_data, type = NULL, regime_threshold = NULL, regime_landmark = NULL, ...) { # nolint
  # cli::cli_alert(c("*" = "predict.floss_regime_model <<- work here"))
  # cli::cli_inform(c("*" = "predict.floss_regime_model: dots_n {rlang::dots_n(...)}"))
  if (rlang::dots_n(...) > 0) { # 0
    cli::cli_alert(c("*" = "predict.floss_regime_model: dots_names {names(rlang::dots_list(..., .preserve_empty = TRUE))}"))
  }
  # type raw came from "numeric", type NULL came from raw directly
  # cli::cli_inform(c("!" = "type is {type}.")) # "raw"
  # cli::cli_inform(c("!" = "regime_threshold is {regime_threshold}.")) # "NULL"
  # cli::cli_inform(c("!" = "regime_landmark is {regime_landmark}.")) # "NULL"

  n <- nrow(new_data)
  if (n == 0) {
    rlang::abort("There are zero rows in the new_data set.")
  }

  new_data <- tibble::tibble(new_data)
  obj_fit <- object$fitted.values
  terms <- object$terms

  # for now, compare:
  checkmate::assert_true(all(new_data$id %in% obj_fit$id))

  if (length(regime_threshold) == 0) { # are we predicting with the old fit?
    regime_threshold <- terms$regime_threshold
  }

  if (length(regime_landmark) == 0) { # are we predicting with the old fit?
    regime_landmark <- terms$regime_landmark
  }

  cli::cli_inform(c("*" = "Predicting regime changes: <<- This is usually fast."))
  cli::cli_inform(c("*" = "Predicting regime changes: threshold {regime_threshold}, landmark {regime_landmark} seconds."))
  cli::cli_inform(c("*" = "Predicting regime changes: number of recordings {length(obj_fit$floss)}."))

  "!DEBUG predicting."

  # if (foreach::getDoParRegistered()) {
  #   cli::cli_inform(c("*" = "Predicting regime changes: using `furrr`."))
  #   estimates <- furrr::future_map(
  #     obj_fit$floss,
  #     floss_predict,
  #     terms$window_size,
  #     terms$time_constraint,
  #     regime_threshold,
  #     regime_landmark,
  #     .options = furrr::furrr_options(seed = TRUE, scheduling = 1, packages = "false.alarm")
  #   )
  # } else {
  cli::cli_inform(c("*" = "Predicting regime changes: using `purrr`."))
  estimates <- purrr::map(
    obj_fit$floss,
    floss_predict,
    terms$window_size,
    terms$time_constraint,
    regime_threshold,
    regime_landmark
  )
  # }

  res <- tibble::tibble(
    .sizes = purrr::map_int(new_data$ts, length),
    .id = as.character(new_data$id),
    .pred = estimates
  )

  res
}



#' Model predictions across many sub-models
#'
#' For some models, predictions can be made on sub-models in the model object.
#' @param object A `model_fit` object.
#' @param new_data A rectangular data object, such as a data frame.
#' @param type A single character value or `NULL`. Possible values are
#' `"numeric"`, `"class"`, `"prob"`, `"conf_int"`, `"pred_int"`, `"quantile"`,
#' or `"raw"`. When `NULL`, `predict()` will choose an appropriate value
#' based on the model's mode.
#' @param ... Optional arguments to pass to `predict.model_fit(type = "raw")`
#'  such as `type`.
#' @return A tibble with the same number of rows as the data being predicted.
#'  There is a list-column named `.pred` that contains tibbles with
#'  multiple rows per sub-model. Note that, within the tibbles, the column names
#'  follow the usual standard based on prediction `type` (i.e. `.pred_class` for
#'  `type = "class"` and so on).
#' @export
multi_predict._floss_regime_model <- function(object, new_data, type = NULL, regime_threshold = NULL, regime_landmark = NULL, ...) { # nolint
  cli::cli_inform(c("!" = "multi_predict._floss_regime_model"))
  # cli::cli_inform(c("*" = "multi_predict._floss_regime_model: dots_n {rlang::dots_n(...)}"))
  if (rlang::dots_n(...) > 0) { # 0
    cli::cli_alert(c("*" = "multi_predict._floss_regime_model: dots_names {names(rlang::dots_list(..., .preserve_empty = TRUE))}"))
  }

  if (any(names(rlang::enquos(...)) == "newdata")) {
    rlang::abort("Did you mean to use `new_data` instead of `newdata`?")
  }

  cli::cli_inform(c("i" = "Multi predicting: <<- This will save us a lot of time!!!"))


  if (is.null(regime_threshold)) {
    regime_threshold <- rlang::eval_tidy(object$fit$terms$regime_threshold)
  }
  ### DONT SORT!!!
  # regime_threshold <- sort(regime_threshold)

  if (is.null(regime_landmark)) {
    regime_landmark <- rlang::eval_tidy(object$fit$terms$regime_landmark)
  }
  ### DONT SORT!!!
  # regime_landmark <- sort(regime_landmark)

  if (is.null(type)) {
    if (object$spec$mode == "regression") {
      type <- "numeric"
    } else {
      type <- "class"
    }
  }

  floss_by_regime_threshold <- function(regime_threshold, regime_landmark, object, new_data, type, ...) {
    object$fit$best.parameters$regime_threshold <- regime_threshold

    checkmate::assert_true(length(regime_threshold) == length(regime_landmark))

    predict(object,
      new_data = new_data, type = "raw", # for now, using raw
      opts = list(regime_threshold = regime_threshold, regime_landmark = regime_landmark), ...
    ) %>%
      dplyr::mutate(regime_threshold = regime_threshold, regime_landmark = regime_landmark, .row = dplyr::row_number()) %>%
      dplyr::select(.row, .sizes, .id, regime_threshold, regime_landmark, dplyr::starts_with(".pred"))
  }

  # do not expand_grid, this is already computed by tune_grid
  # multi_args <- tidyr::expand_grid(regime_threshold, regime_landmark)

  res <- purrr::map2_df(regime_threshold, regime_landmark,
    floss_by_regime_threshold,
    object = object,
    new_data = new_data,
    type = type,
    ...
  )
  res <- dplyr::arrange(res, .row, regime_threshold, regime_landmark)
  res <- split(res[, -1], res$.row)
  names(res) <- NULL
  dplyr::tibble(.pred = res)
}
