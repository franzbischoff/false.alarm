source(here::here("scripts", "common", "score_floss.R"), local = .GlobalEnv, encoding = "UTF-8")

# rlang::local_use_cli(format = TRUE, inline = TRUE, frame = caller_env())

# options(cli.user_theme = list(
#   span.code = list(
#     "background-color" = "#3B4252",
#     color = "#E5E9F0"
#   )
# ))

## Tuning parameters

time_constraint_par <- function(range = c(750L, 5000L), trans = trans_round(-2)) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    default = 5000L,
    label = c(time_constraint = "Time Constraint"),
    finalize = NULL
  )
}

window_size_par <- function(range = c(150L, 250L), trans = trans_round(-1)) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    default = 200,
    values = seq.int(range[1], range[2]) %>% round(-1),
    label = c(window_size = "Window Size"),
    finalize = NULL
  )
}
trans_round <- function(d = 1) {
  force(d)

  roundi_trans <- function(x) {
    round(x, d)
  }

  round_trans <- function(x) {
    round(x, d)
  }

  scales::trans_new(
    "trans_round",
    transform = "round_trans",
    inverse = "roundi_trans",
    domain = c(0, Inf)
  )
}

mp_threshold_par <- function(range = c(0, 1), trans = trans_round()) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    default = 0.2,
    label = c(threshold = "MP Threshold"),
    finalize = NULL
  )
}

## Main interface

floss_regime_model <- function(mode = "regression",
                               window_size = NULL, time_constraint = NULL,
                               mp_threshold = NULL, regime_threshold = NULL, engine = "floss") {
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
    regime_threshold = rlang::enquo(regime_threshold)
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

## Training interface

# verbose
train_regime_model <- function(truth, ts, ..., window_size, regime_threshold, mp_threshold, time_constraint) {
  cli::cli_alert(c("!" = "train_regime_model <<- work here"))
  cli::cli_inform(c("*" = "train_regime_model: dots_n {rlang::dots_n(...)}"))
  if (rlang::dots_n(...) > 0) { # 0
    cli::cli_inform(c("*" = "train_regime_model: dots_names {names(rlang::dots_list(..., .preserve_empty = TRUE))}"))
  }
  dots <- rlang::dots_list(...)

  n <- nrow(ts)
  if (n == 0) {
    rlang::abort("There are zero rows in the predictor set.")
  }

  "!DEBUG fitting model."

  if (ncol(ts) > 1) {
    id <- ts[[1]]
    ts <- ts[[2]]
  } else {
    id <- seq.int(1, n)
  }

  res <- list(truth = truth, id = id, ts = ts)

  trained <- list(
    fitted.values = res,
    terms = list(
      window_size = window_size,
      regime_threshold = regime_threshold,
      mp_threshold = mp_threshold,
      time_constraint = time_constraint
    )
  )

  class(trained) <- "floss_regime_model"

  return(trained)
}

# Predictor interface

# object = object$fit
# new_data = new_data
# type = "numeric"
# regime_threshold = object$spec$args$regime_threshold
# verbose = FALSE
predict.floss_regime_model <- function(object, new_data, type = NULL, regime_threshold = NULL, ...) { # nolint
  cli::cli_inform(c("*" = "predict.floss_regime_model <<- work here"))
  cli::cli_inform(c("*" = "predict.floss_regime_model: dots_n {rlang::dots_n(...)}"))
  if (rlang::dots_n(...) > 0) { # 0
    cli::cli_inform(c("*" = "predict.floss_regime_model: dots_names {names(rlang::dots_list(..., .preserve_empty = TRUE))}"))
  }
  cli::cli_inform(c("!" = "type is {type}.")) # "raw"
  cli::cli_inform(c("!" = "regime_threshold is {regime_threshold}.")) # "NULL"

  n <- nrow(new_data)
  if (n == 0) {
    rlang::abort("There are zero rows in the new_data set.")
  }

  dots <- rlang::dots_list(...)

  new_data <- tibble::tibble(new_data)
  obj_fit <- tibble::as_tibble(object$fitted.values) %>% dplyr::mutate(id = as.character(id))
  terms <- object$terms

  # for now, compare:
  checkmate::assert_true(all(new_data$id %in% obj_fit$id))

  if (length(regime_threshold) == 0) { # are we predicting with the old fit?
    regime_threshold <- terms$regime_threshold
  }

  # TODO: create real estimates
  estimates <- obj_fit$truth

  if (length(regime_threshold) == 1) { # new predict with single value
    cli::cli_inform(c("*" = "predict.floss_regime_model <<- single prediction"))
    debug <- abs((100 - terms$window_size) / 10)
    debug <- debug + abs(0.3 - regime_threshold) * 10
    debug <- debug + abs(0.5 - terms$mp_threshold) * 10
    debug <- debug + abs((1000 - terms$time_constraint) / 100)
    res <- tibble::tibble(
      .sizes = purrr::map_int(new_data$ts, length),
      .id = as.character(new_data$id),
      # regime_threshold = regime_threshold,
      .pred = purrr::map(estimates, ~ .x + debug),
    )
  } else { # new multiple values
    cli::cli_inform(c("*" = "predict.floss_regime_model <<- multi prediction"))
    res <- NULL
    for (i in seq_len(length(new_data$ts))) {
      new_res <- NULL
      for (rt in regime_threshold) {
        debug <- abs((100 - terms$window_size) / 10)
        debug <- debug + abs(0.3 - rt) * 10
        debug <- debug + abs(0.5 - terms$mp_threshold) * 10
        debug <- debug + abs((1000 - terms$time_constraint) / 100)
        new_res <- dplyr::bind_rows(
          new_res,
          tibble::tibble(
            .pred = list(estimates[[i]] + debug),
            .sizes = length(new_data$ts[[i]]),
            .id = as.character(new_data$id[i]),
            regime_threshold = rt
          )
        )
      }

      res <- dplyr::bind_rows(res, tidyr::nest(new_res, .pred = tidyr::everything()))
    }
  }

  res
}

# predict._floss_regime_model <- function(object, new_data, type = NULL, opts = list(),
#                                         regime_threshold = NULL, multi = FALSE, ...) { # nolint
#   cli::cli_inform(c("*" = "predict._floss_regime_model"))
#   cli::cli_inform(c("*" = "predict._floss_regime_model: dots_n {rlang::dots_n(...)}"))
#   if (rlang::dots_n(...) > 0) {
#     cli::cli_inform(c("*" = "predict._floss_regime_model: dots_names {names(rlang::dots_list(..., .preserve_empty = TRUE))}"))
#   }

#   if (any(names(rlang::enquos(...)) == "newdata")) {
#     rlang::abort("Did you mean to use `new_data` instead of `newdata`?")
#   }

#   # See discussion in https://github.com/tidymodels/parsnip/issues/195
#   if (is.null(regime_threshold) && !is.null(object$spec$args$regime_threshold)) {
#     cli::cli_inform(c("!" = "github._floss_regime_model"))
#     regime_threshold <- object$spec$args$regime_threshold
#   }

#   cli::cli_inform(c("!" = "type is {type}."))
#   cli::cli_inform(c("!" = "length(opts) is {length(opts)}."))
#   cli::cli_inform(c("!" = "regime_threshold is {regime_threshold}."))
#   cli::cli_inform(c("!" = "multi is {multi}."))

#   cli::cli_inform(c("!" = "object$spec$args$regime_threshold is {object$spec$args$regime_threshold}."))
#   cli::cli_inform(c("!" = "object$fit$terms$regime_threshold is {object$fit$terms$regime_threshold}."))
#   # opts$multi <- multi


#   object$spec$args$regime_threshold <- .check_floss_regime_threshold_predict(regime_threshold, object, multi)

#   cli::cli_inform(c("!" = "new object$spec$args$regime_threshold is {object$spec$args$regime_threshold}."))

#   object$spec <- eval_args(object$spec)
#   # check_args.floss_regime_model(object$spec)
#   predict.model_fit(object, new_data = new_data, type = type, ...) # , opts = opts
# }


# predict_numeric._floss_regime_model <- function(object, new_data, ...) { # nolint
#   cli::cli_inform(c("*" = "predict_numeric._floss_regime_model"))
#   cli::cli_inform(c("*" = "predict_numeric._floss_regime_model: dots_n {rlang::dots_n(...)}"))
#   if (rlang::dots_n(...) > 0) {
#     cli::cli_inform(c("*" = "predict_numeric._floss_regime_model: dots_names {names(rlang::dots_list(..., .preserve_empty = TRUE))}"))
#   }

#   if (any(names(rlang::enquos(...)) == "newdata")) {
#     rlang::abort("Did you mean to use `new_data` instead of `newdata`?")
#   }

#   object$spec <- eval_args(object$spec)
#   predict_numeric.model_fit(object, new_data = new_data, ...)
# }

# predict_raw._floss_regime_model <- function(object, new_data, opts = list(), ...) { # nolint
#   cli::cli_inform(c("*" = "predict_raw._floss_regime_model"))
#   cli::cli_inform(c("*" = "predict_raw._floss_regime_model: dots_n {rlang::dots_n(...)}"))
#   if (rlang::dots_n(...) > 0) {
#     cli::cli_inform(c("*" = "predict_raw._floss_regime_model: dots_names {names(rlang::dots_list(..., .preserve_empty = TRUE))}"))
#   }
#   cli::cli_inform(c("!" = "length(opts) is {length(opts)}."))

#   # browser()

#   if (any(names(rlang::enquos(...)) == "newdata")) {
#     rlang::abort("Did you mean to use `new_data` instead of `newdata`?")
#   }

#   object$spec <- eval_args(object$spec)
#   opts$regime_threshold <- object$spec$args$regime_threshold
#   predict_raw.model_fit(object, new_data = new_data, opts = opts, ...)
# }

## Multipredict

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

multi_predict._floss_regime_model <- function(object, new_data, type = NULL, regime_threshold = NULL, ...) { # nolint
  cli::cli_inform(c("*" = "multi_predict._floss_regime_model"))
  cli::cli_inform(c("*" = "multi_predict._floss_regime_model: dots_n {rlang::dots_n(...)}"))
  if (rlang::dots_n(...) > 0) { # 0
    cli::cli_inform(c("*" = "multi_predict._floss_regime_model: dots_names {names(rlang::dots_list(..., .preserve_empty = TRUE))}"))
  }

  if (any(names(rlang::enquos(...)) == "newdata")) {
    rlang::abort("Did you mean to use `new_data` instead of `newdata`?")
  }

  if (is.null(regime_threshold)) {
    regime_threshold <- rlang::eval_tidy(object$fit$terms$regime_threshold)
  }
  regime_threshold <- sort(regime_threshold)

  if (is.null(type)) {
    if (object$spec$mode == "regression") {
      type <- "numeric"
    } else {
      type <- "class"
    }
  }

  floss_by_regime_threshold <- function(regime_threshold, object, new_data, type, ...) {
    object$fit$best.parameters$regime_threshold <- regime_threshold

    predict(object, new_data = new_data, type = type, ...) %>%
      dplyr::mutate(regime_threshold = regime_threshold, .row = dplyr::row_number()) %>%
      dplyr::select(.row, .sizes, .id, regime_threshold, dplyr::starts_with(".pred"))
  }

  res <- purrr::map_df(regime_threshold,
    floss_by_regime_threshold,
    object = object,
    new_data = new_data,
    type = type,
    ...
  )
  res <- dplyr::arrange(res, .row, regime_threshold)
  res <- split(res[, -1], res$.row)
  names(res) <- NULL
  dplyr::tibble(.pred = res)

  # dots <- rlang::dots_list(...)

  # if (length(dots) > 0) {
  #   bad_args <- names(dots)
  #   bad_args <- paste0("`", bad_args, "`", collapse = ", ")
  #   rlang::abort(
  #     glue::glue(
  #       "These arguments cannot be used: {bad_args}. The ellipses are not ",
  #       "used to pass args to the model function's predict function.",
  #     )
  #   )
  # }

  # object$spec <- eval_args(object$spec)
  # if (is.null(regime_threshold)) {
  #   # See discussion in https://github.com/tidymodels/parsnip/issues/195
  #   if (!is.null(object$spec$args$regime_threshold)) {
  #     cli::cli_inform(c("!" = "github.object$spec$args$regime_threshold"))
  #     regime_threshold <- object$spec$args$regime_threshold
  #   } else {
  #     cli::cli_inform(c("!" = "github.object$fit$terms$regime_threshold"))
  #     regime_threshold <- object$fit$terms$regime_threshold
  #   }
  # }

  # pred <- predict._floss_regime_model(object,
  #   new_data = new_data, type = "raw",
  #   opts = dots, regime_threshold = regime_threshold,
  #   multi = TRUE
  # )

  # browser()

  # param_key <- tibble(group = colnames(pred), regime_threshold = regime_threshold)
  # pred <- as_tibble(pred)
  # pred$.row <- seq_len(nrow(pred))
  # pred <- gather(pred, group, .pred, -.row)
  # pred <- full_join(param_key, pred, by = "group")
  # pred$group <- NULL
  # pred <- arrange(pred, .row, regime_threshold)
  # .row <- pred$.row
  # pred$.row <- NULL
  # pred <- split(pred, .row)
  # names(pred) <- NULL
  # cli::cli_inform(c("!" = "RAW result"))
  # tibble(.pred = pred)
  # pred
}

# multi_predict.floss_regime_model <- multi_predict._floss_regime_model

print.floss_regime_model <- function(x, ...) { # nolint
  cat("FLOSS Regime Model Specification (", x$mode, ")\n\n", sep = "")
  parsnip::model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(parsnip::show_call(x))
  }

  invisible(x)
}


update.floss_regime_model <- function(object,
                                      parameters = NULL,
                                      window_size = NULL,
                                      time_constraint = NULL,
                                      mp_threshold = NULL,
                                      regime_threshold = NULL,
                                      fresh = FALSE, ...) { # nolint
  cli::cli_inform(c("*" = "update.floss_regime_model"))
  cli::cli_inform(c("*" = "update.floss_regime_model: dots_n {rlang::dots_n(...)}"))
  if (rlang::dots_n(...) > 0) {
    cli::cli_inform(c("*" = "update.floss_regime_model: dots_names {names(rlang::dots_list(..., .preserve_empty = TRUE))}"))
  }

  args <- list(
    window_size = rlang::enquo(window_size),
    time_constraint = rlang::enquo(time_constraint),
    mp_threshold = rlang::enquo(mp_threshold),
    regime_threshold = rlang::enquo(regime_threshold)
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
      null_args <- purrr::map_lgl(args, null_value)
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

min_grid.floss_regime_model <- function(x, grid, ...) { # nolint
  cli::cli_inform(c("*" = "min_grid.floss_regime_model "))
  cli::cli_inform(c("*" = "min_grid.floss_regime_model: dots_n {rlang::dots_n(...)}"))
  if (rlang::dots_n(...) > 0) { # 0
    cli::cli_inform(c("*" = "min_grid.floss_regime_model: dots_names {names(rlang::dots_list(..., .preserve_empty = TRUE))}"))
  }

  # This is basically `fit_max_value()` with an extra error trap
  gr_nms <- names(grid)
  param_info <- tune:::get_submodel_info(x)
  sub_nm <- param_info$id[param_info$has_submodel]

  if (length(sub_nm) == 0) {
    return(tune:::blank_submodels(grid))
  }

  fixed_args <- gr_nms[gr_nms != sub_nm]

  if (length(fixed_args) == 0) {
    res <- tune:::submod_only(grid)
  } else {
    res <- tune:::submod_and_others(grid, fixed_args)
  }
  res
}

check_args.floss_regime_model <- function(object) { # nolint

  "!DEBUG This runs in fit()"
  cli::cli_inform(c("*" = "check_args.floss_regime_model"))

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


translate.floss_regime_model <- function(x, engine = x$engine, ...) { # nolint
  cli::cli_inform(c("*" = "translate.floss_regime_model"))
  cli::cli_inform(c("*" = "translate.floss_regime_model: dots_n {rlang::dots_n(...)}"))
  if (rlang::dots_n(...) > 0) { # 0
    cli::cli_inform(c("*" = "translate.floss_regime_model: dots_names {names(rlang::dots_list(..., .preserve_empty = TRUE))}"))
  }

  if (is.null(engine)) {
    message("Used `engine = 'floss'` for translation.")
    engine <- "floss"
  }

  x <- translate.default(x, engine, ...)

  if (engine == "floss") {
    .check_floss_regime_threshold_fit(x)
  }
  x
}

# ------------------------------------------------------------------------------
# glmnet call stack for linear regression using `predict` when object has
# classes "_elnet" and "model_fit":
#
#  predict()
# 	predict._elnet(penalty = NULL)   <-- checks and sets penalty
#    predict.model_fit()             <-- checks for extra vars in ...
#     predict_numeric()
#      predict_numeric._elnet()
#       predict_numeric.model_fit()
#        predict.elnet()


# glmnet call stack for linear regression using `multi_predict` when object has
# classes "_elnet" and "model_fit":
#
# 	multi_predict()
#    multi_predict._elnet(penalty = NULL)
#      predict._elnet(multi = TRUE)          <-- checks and sets penalty
#       predict.model_fit()                  <-- checks for extra vars in ...
#        predict_raw()
#         predict_raw._elnet()
#          predict_raw.model_fit(opts = list(s = penalty))
#           predict.elnet()

#' Organize FLOSS predictions
#'
#' This function is for developer use and organizes predictions from floss
#' models.
#'
#' @param x Predictions as returned by the `predict()` method for floss models.
#' @param object An object of class `model_fit`.
#'
#' @rdname floss_helpers_prediction
#' @keywords internal
#' @export
.organize_floss_regime_pred <- function(x, object) {
  cli::cli_inform(c("*" = ".organize_floss_regime_pred"))

  n <- nrow(x)

  if (!is.null(object$spec$args$regime_threshold)) {
    regime_threshold <- rep(object$spec$args$regime_threshold, each = n)
  } else {
    regime_threshold <- rep(object$fit$terms$regime_threshold, each = n)
  }

  res <- dplyr::bind_cols(id = seq_len(n), regime_threshold = regime_threshold, x)
  res <- res %>%
    nest(.pred = !id) %>%
    select(!id)
  res
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
  cli::cli_inform(c("*" = ".check_floss_regime_threshold_fit"))
  regime_threshold <- rlang::eval_tidy(x$args$regime_threshold)

  if (length(regime_threshold) != 1) {
    rlang::abort(c(
      "For the floss engine, `regime_threshold` must be a single number (or a value of `tune()`).",
      glue::glue("There are {length(regime_threshold)} values for `regime_threshold`."),
      "To try multiple values for total regularization, use the tune package.",
      "To predict multiple regime_threshold, use `multi_predict()`"
    ))
  }
}

#' @param regime_threshold A regime_threshold value to check.
#' @param object An object of class `model_fit`.
#' @param multi A logical indicating if multiple values are allowed.
#'
#' @rdname floss_helpers
#' @keywords internal
#' @export
.check_floss_regime_threshold_predict <- function(regime_threshold = NULL, object, multi = FALSE) { # nolint
  if (is.null(regime_threshold)) {
    regime_threshold <- object$fit$terms$regime_threshold
  }

  if (length(object$fit$terms$window_size) > 1) {
    rlang::abort("window_size must be a single value")
  }
  if (length(object$fit$terms$mp_threshold) > 1) {
    rlang::abort("mp_threshold must be a single value")
  }
  if (length(object$fit$terms$time_constraint) > 1) {
    rlang::abort("time_constraint must be a single value")
  }

  # when using `predict()`, allow for a single regime_threshold
  if (!multi) {
    if (length(regime_threshold) != 1) {
      rlang::abort(
        glue::glue(
          "`regime_threshold` should be a single numeric value. `multi_predict()` ",
          "can be used to get multiple predictions per row of data.",
        )
      )
    }
  }

  regime_threshold
}

## Register parsnip model

if (!any(parsnip::get_model_env()$models == "floss_regime_model")) {
  parsnip::set_new_model("floss_regime_model")
  parsnip::set_model_mode(model = "floss_regime_model", mode = "regression")
  parsnip::set_model_engine(
    "floss_regime_model",
    mode = "regression",
    eng = "floss"
  )
}

parsnip::set_dependency("floss_regime_model", eng = "floss") # pkg = "false.alarm"

parsnip::set_model_arg(
  model = "floss_regime_model",
  eng = "floss",
  parsnip = "window_size",
  original = "window_size",
  func = list(fun = "window_size_par"),
  has_submodel = FALSE
)

parsnip::set_model_arg(
  model = "floss_regime_model",
  eng = "floss",
  parsnip = "mp_threshold",
  original = "mp_threshold",
  func = list(fun = "mp_threshold_par"),
  has_submodel = FALSE
)

parsnip::set_model_arg(
  model = "floss_regime_model",
  eng = "floss",
  parsnip = "time_constraint",
  original = "time_constraint",
  func = list(fun = "time_constraint_par"),
  has_submodel = FALSE
)

parsnip::set_model_arg(
  model = "floss_regime_model",
  eng = "floss",
  parsnip = "regime_threshold",
  original = "regime_threshold",
  func = list(pkg = "dials", fun = "threshold", range = c(0, 1)),
  has_submodel = TRUE
)

# parsnip::set_fit(
#   model = "floss_regime_model",
#   eng = "floss",
#   mode = "regression",
#   value = list(
#     interface = "formula",
#     protect = c("formula", "data"),
#     func = c(fun = "train_regime_model"),
#     defaults = list(verbose = FALSE)
#   )
# )

parsnip::set_fit(
  model = "floss_regime_model",
  eng = "floss",
  mode = "regression",
  value = list(
    interface = "data.frame",
    data = c(x = "ts", y = "truth", id = "id"), # ts$x[[2]][1:10] regimes[[2]]
    protect = c("ts", "truth"),
    func = c(fun = "train_regime_model"),
    defaults = list() # verbose = FALSE
  )
)

parsnip::set_encoding(
  model = "floss_regime_model",
  eng = "floss",
  mode = "regression",
  options = list(
    predictor_indicators = "none",
    compute_intercept = FALSE,
    remove_intercept = FALSE,
    allow_sparse_x = FALSE
  )
)


### Prediction parameters

parsnip::set_pred(
  model = "floss_regime_model",
  eng = "floss",
  mode = "regression",
  type = "numeric",
  value = parsnip::pred_value_template(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    # Now everything else is put into the `args` slot
    object = rlang::expr(object$fit), # or quote()?
    new_data = rlang::expr(new_data), # or quote()?
    type = "raw"
    # type = "numeric",
    # regime_threshold = rlang::expr(object$spec$args$regime_threshold),
    # verbose = FALSE
  )
)

parsnip::set_pred(
  model = "floss_regime_model",
  eng = "floss",
  mode = "regression",
  type = "raw",
  value = parsnip::pred_value_template(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    # Now everything else is put into the `args` slot
    object = rlang::expr(object$fit), # or quote()?
    new_data = rlang::expr(new_data) # or quote()?
  )
)

## Yardstick custom metric

floss_error <- function(data, ...) {
  cli::cli_inform(c("*" = "floss_error <<- work here"))
  UseMethod("floss_error")
}

floss_error <- yardstick::new_numeric_metric(floss_error, direction = "minimize")

floss_error_micro <- yardstick::metric_tweak("floss_error_micro", floss_error, estimator = "micro")
floss_error_macro <- yardstick::metric_tweak("floss_error_macro", floss_error, estimator = "macro")

floss_error.data.frame <- function(data, truth, estimate, na_rm = TRUE, estimator = "binary", ...) { # nolint
  cli::cli_inform(c("*" = "floss_error.data.frame <<- work here"))
  cli::cli_inform(c("*" = "floss_error.data.frame: dots_n {rlang::dots_n(...)}"))
  if (rlang::dots_n(...) > 0) { # 0
    cli::cli_inform(c("*" = "floss_error.data.frame: dots_names {names(rlang::dots_list(..., .preserve_empty = TRUE))}"))
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
  cli::cli_inform(c("*" = "floss_error_vec <<- work here"))
  cli::cli_inform(c("*" = "floss_error_vec: dots_n {rlang::dots_n(...)}"))
  if (rlang::dots_n(...) > 0) {
    cli::cli_inform(c("*" = "floss_error_vec: dots_names {names(rlang::dots_list(..., .preserve_empty = TRUE))}"))
  }

  floss_error_impl <- function(truth, estimate, data_size, ...) {
    res <- score_regimes(truth, estimate, data_size)
    return(res)
  }

  cli::cli_inform(c("*" = "data_size len: {length(data_size)}"))

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

  if (estimator == "micro") {
    cli::cli_inform(c("*" = "floss_error_vec <<- micro"))
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
    cli::cli_inform(c("*" = "floss_error_vec <<- macro"))
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

clean_splits_data <- function(object) {
  tidy_splits <- object$splits
  tidy_splits <- purrr::map(tidy_splits, function(x) {
    x$data$ts <- NA
    x
  })
  object$splits <- tidy_splits
  object
}

# floss_error(tidy_dataset, truth = truth, estimate = truth, estimator = "micro")$.estimate
