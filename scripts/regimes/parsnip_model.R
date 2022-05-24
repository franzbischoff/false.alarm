source(here::here("scripts", "common", "score_floss.R"), local = .GlobalEnv, encoding = "UTF-8")
options(dplyr.summarise.inform = FALSE)

# rlang::local_use_cli(format = TRUE, inline = TRUE, frame = caller_env())

# options(cli.user_theme = list(
#   span.code = list(
#     "background-color" = "#3B4252",
#     color = "#E5E9F0"
#   )
# ))

## Tuning parameters

time_constraint_par <- function(range = c(750L, 5000L), trans = trans_round(50)) {
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

window_size_par <- function(range = c(150L, 250L), trans = trans_round(25)) {
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

  d <- abs(d)

  if (d == 0) {
    cli::cli_warn(c("!" = "Rounding value 0 is invalid and was changed to 1."))
    d <- 1
  }

  roundi_trans <- function(x) {
    x + runif(1, -1, 1) * (d / 2)
  }

  round_trans <- function(x) {
    round(x / d) * d
  }

  scales::trans_new(
    "trans_round",
    transform = "roundi_trans",
    inverse = "round_trans",
    domain = c(0, Inf)
  )
}

mp_threshold_par <- function(range = c(0, 1), trans = trans_round(0.1)) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    default = 0,
    label = c(mp_threshold = "MP Threshold"),
    finalize = NULL
  )
}

regime_threshold_par <- function(range = c(0, 1), trans = trans_round(0.1)) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    default = 0.3,
    label = c(regime_threshold = "Regime Threshold"),
    finalize = NULL
  )
}

regime_landmark_par <- function(range = c(1, 10), trans = trans_round(0.5)) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    default = 3,
    label = c(regime_landmark = "Regime Landmark (s)"),
    finalize = NULL
  )
}

## Main interface

floss_regime_model <- function(mode = "regression",
                               window_size = NULL, time_constraint = NULL,
                               mp_threshold = NULL, regime_threshold = NULL, regime_landmark = NULL, engine = "floss") {
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

## Training interface

# verbose
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

  gc(verbose = FALSE)
  # floss <- purrr::map(
  #   ts,
  #   training_regimes,
  #   window_size,
  #   mp_threshold,
  #   time_constraint
  # )
  res$floss <- floss

  trained <- list(
    fitted.values = as_tibble(res),
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

# Predictor interface

# object = object$fit
# new_data = new_data
# type = "numeric"
# regime_threshold = object$spec$args$regime_threshold
# verbose = FALSE
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

  dots <- rlang::dots_list(...)

  new_data <- tibble::tibble(new_data)
  # obj_fit <- tibble::as_tibble(object$fitted.values) %>% dplyr::mutate(id = as.character(id))
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
  cli::cli_inform(c("*" = "Training the model: number of recordings {length(obj_fit$floss)}."))

  "!DEBUG predicting."

  # estimates <- furrr::future_map(obj_fit$floss, predict_regimes,
  #   terms$window_size,
  #   terms$time_constraint,
  #   regime_threshold,
  #   regime_landmark,
  #   .options = furrr::furrr_options(seed = TRUE, scheduling = 1)
  # )

  cli::cli_inform(c("*" = "Predicting regime changes: using `purrr`."))
  estimates <- purrr::map(
    obj_fit$floss, predict_regimes,
    terms$window_size,
    terms$time_constraint,
    regime_threshold,
    regime_landmark
  )

  res <- tibble::tibble(
    .sizes = purrr::map_int(new_data$ts, length),
    .id = as.character(new_data$id),
    .pred = estimates,
  )

  res
}

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

multi_predict._floss_regime_model <- function(object, new_data, type = NULL, regime_threshold = NULL, regime_landmark = NULL, ...) { # nolint
  # cli::cli_warn(c("x" = "multi_predict._floss_regime_model"))
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
  regime_threshold <- sort(regime_threshold)

  if (is.null(regime_landmark)) {
    regime_landmark <- rlang::eval_tidy(object$fit$terms$regime_landmark)
  }
  regime_landmark <- sort(regime_landmark)

  if (is.null(type)) {
    if (object$spec$mode == "regression") {
      type <- "numeric"
    } else {
      type <- "class"
    }
  }

  floss_by_regime_threshold <- function(regime_threshold, regime_landmark, object, new_data, type, ...) {
    object$fit$best.parameters$regime_threshold <- regime_threshold

    predict(object,
      new_data = new_data, type = "raw", # for now, using raw
      opts = list(regime_threshold = regime_threshold, regime_landmark = regime_landmark), ...
    ) %>%
      dplyr::mutate(regime_threshold = regime_threshold, regime_landmark = regime_landmark, .row = dplyr::row_number()) %>%
      dplyr::select(.row, .sizes, .id, regime_threshold, regime_landmark, dplyr::starts_with(".pred"))
  }

  multi_args <- tidyr::expand_grid(regime_threshold, regime_landmark)

  res <- purrr::map2_df(multi_args$regime_threshold, multi_args$regime_landmark,
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

# hack tune

tune_grid_loop_iter <- function(split,
                                grid_info,
                                workflow,
                                metrics,
                                control,
                                seed) {
  tune::load_pkgs(workflow)

  load_namespace <- function(x) {
    if (length(x) == 0) {
      return(invisible(TRUE))
    }

    x_full <- x[x %in% full_load]
    x <- x[!(x %in% full_load)]

    loaded <- purrr::map_lgl(x, isNamespaceLoaded)
    x <- x[!loaded]

    if (length(x) > 0) {
      did_load <- purrr::map_lgl(x, requireNamespace, quietly = TRUE)
      if (any(!did_load)) {
        bad <- x[!did_load]
        msg <- paste0("'", bad, "'", collapse = ", ")
        rlang::abort(paste("These packages could not be loaded:", msg))
      }
    }

    if (length(x_full) > 0) {
      purrr::map(
        x_full,
        ~ try(suppressPackageStartupMessages(attachNamespace(.x)), silent = TRUE)
      )
    }

    invisible(TRUE)
  }
  load_namespace(control$pkgs)

  # After package loading to avoid potential package RNG manipulation
  if (!is.null(seed)) {
    # `assign()`-ing the random seed alters the `kind` type to L'Ecuyer-CMRG,
    # so we have to ensure it is restored on exit
    old_kind <- RNGkind()[[1]]
    assign(".Random.seed", seed, envir = globalenv())
    on.exit(RNGkind(kind = old_kind), add = TRUE)
  }

  control_parsnip <- parsnip::control_parsnip(verbosity = 0, catch = TRUE)
  control_workflow <- workflows::control_workflow(control_parsnip = control_parsnip)

  event_level <- control$event_level

  out_metrics <- NULL
  out_extracts <- NULL
  out_predictions <- NULL
  out_all_outcome_names <- list()
  out_notes <-
    tibble::tibble(location = character(0), type = character(0), note = character(0))

  params <- hardhat::extract_parameter_set_dials(workflow)
  model_params <- dplyr::filter(params, source == "model_spec")
  preprocessor_params <- dplyr::filter(params, source == "recipe")

  param_names <- dplyr::pull(params, "id")
  model_param_names <- dplyr::pull(model_params, "id")
  preprocessor_param_names <- dplyr::pull(preprocessor_params, "id")

  # Model related grid-info columns
  cols <- rlang::expr(
    c(
      .iter_model,
      .iter_config,
      .msg_model,
      dplyr::all_of(model_param_names),
      .submodels
    )
  )

  # Nest grid_info:
  # - Preprocessor info in the outer level
  # - Model info in the inner level
  if (tune:::tidyr_new_interface()) {
    grid_info <- tidyr::nest(grid_info, data = !!cols)
  } else {
    grid_info <- tidyr::nest(grid_info, !!cols)
  }

  training <- rsample::analysis(split)

  # ----------------------------------------------------------------------------
  # Preprocessor loop

  iter_preprocessors <- grid_info[[".iter_preprocessor"]]

  workflow_original <- workflow

  for (iter_preprocessor in iter_preprocessors) {
    workflow <- workflow_original

    iter_grid_info <- dplyr::filter(
      .data = grid_info,
      .iter_preprocessor == iter_preprocessor
    )

    iter_grid_preprocessor <- dplyr::select(
      .data = iter_grid_info,
      dplyr::all_of(preprocessor_param_names)
    )

    iter_msg_preprocessor <- iter_grid_info[[".msg_preprocessor"]]


    workflow <- tune:::finalize_workflow_preprocessor(
      workflow = workflow,
      grid_preprocessor = iter_grid_preprocessor
    )

    workflow <- tune:::catch_and_log(
      .expr = .fit_pre(workflow, training),
      control,
      split,
      iter_msg_preprocessor,
      notes = out_notes
    )

    if (tune:::is_failure(workflow)) {
      next
    }

    # --------------------------------------------------------------------------
    # Model loop

    iter_grid_info_models <- iter_grid_info[["data"]][[1L]]
    iter_models <- iter_grid_info_models[[".iter_model"]]

    workflow_preprocessed <- workflow

    for (iter_model in iter_models) {
      workflow <- workflow_preprocessed

      iter_grid_info_model <- dplyr::filter(
        .data = iter_grid_info_models,
        .iter_model == iter_model
      )

      iter_grid_model <- dplyr::select(
        .data = iter_grid_info_model,
        dplyr::all_of(model_param_names)
      )

      iter_submodels <- iter_grid_info_model[[".submodels"]][[1L]]
      iter_msg_model <- iter_grid_info_model[[".msg_model"]]
      iter_config <- iter_grid_info_model[[".iter_config"]][[1L]]

      workflow <- tune:::finalize_workflow_spec(workflow, iter_grid_model)

      workflow <- tune:::catch_and_log_fit(
        expr = .fit_model(workflow, control_workflow),
        control,
        split,
        iter_msg_model,
        notes = out_notes
      )

      # Check for parsnip level and model level failure
      if (tune:::is_failure(workflow) || tune:::is_failure(workflow$fit$fit$fit)) {
        next
      }

      workflow <- .fit_finalize(workflow)

      # Extract outcome names from the hardhat mold
      outcome_names <- tune:::outcome_names(workflow)

      out_all_outcome_names <- tune:::append_outcome_names(
        all_outcome_names = out_all_outcome_names,
        outcome_names = outcome_names
      )


      # FIXME: I think this might be wrong? Doesn't use submodel parameters,
      # so `extracts` column doesn't list the correct parameters.
      iter_grid <- dplyr::bind_cols(
        iter_grid_preprocessor,
        iter_grid_model
      )

      # FIXME: bind_cols() drops number of rows with zero col data frames
      # because of a bug with vec_cbind()
      # https://github.com/r-lib/vctrs/issues/1281
      if (ncol(iter_grid_preprocessor) == 0L && ncol(iter_grid_model) == 0L) {
        nrow <- nrow(iter_grid_model)
        iter_grid <- tibble::new_tibble(x = list(), nrow = nrow)
      }

      out_extracts <- tune:::append_extracts(
        collection = out_extracts,
        workflow = workflow,
        grid = iter_grid,
        split = split,
        ctrl = control,
        .config = iter_config
      )

      iter_msg_predictions <- paste(iter_msg_model, "(predictions)")

      iter_predictions <- tune:::catch_and_log(
        predict_model(split, workflow, iter_grid, metrics, iter_submodels),
        control,
        split,
        iter_msg_predictions,
        bad_only = TRUE,
        notes = out_notes
      )

      # Check for prediction level failure
      if (tune:::is_failure(iter_predictions)) {
        next
      }
      # browser() # BINGO
      out_metrics <- tune:::append_metrics(
        collection = out_metrics,
        predictions = iter_predictions,
        metrics = metrics,
        param_names = param_names,
        outcome_name = outcome_names,
        event_level = event_level,
        split = split,
        .config = iter_config
      )

      iter_config_metrics <- tune:::extract_metrics_config(param_names, out_metrics)

      out_predictions <- tune:::append_predictions(
        collection = out_predictions,
        predictions = iter_predictions,
        split = split,
        control = control,
        .config = iter_config_metrics
      )
    } # model loop
  } # preprocessor loop

  list(
    .metrics = out_metrics,
    .extracts = out_extracts,
    .predictions = out_predictions,
    .all_outcome_names = out_all_outcome_names,
    .notes = out_notes
  )
}


append_metrics <- function(collection,
                           predictions,
                           metrics,
                           param_names,
                           outcome_name,
                           event_level,
                           split,
                           .config = NULL) {
  if (inherits(predictions, "try-error")) {
    return(collection)
  }

  tmp_est <- tune:::estimate_metrics(
    dat = predictions,
    metric = metrics,
    param_names = param_names,
    outcome_name = outcome_name,
    event_level = event_level
  )

  tmp_est <- cbind(tmp_est, labels(split))

  if (!rlang::is_null(.config)) {
    if (nrow(tmp_est) != length(.config)) {
      cli::cli_warn("nrow(tmp_est) = {tmp_est}, length(.config) = {length(.config)}.")
    }

    tmp_est <- cbind(tmp_est, .config) # fix here , 7 rows for 2 config values
  }

  dplyr::bind_rows(collection, tmp_est)
}


predict_model <- function(split, workflow, grid, metrics, submodels = NULL) {
  model <- extract_fit_parsnip(workflow)

  # new_data <- rsample::assessment(split)
  forged <- tune:::forge_from_workflow(split, workflow)
  x_vals <- forged$predictors
  y_vals <- forged$outcomes

  orig_rows <- as.integer(split, data = "assessment")

  if (length(orig_rows) != nrow(x_vals)) {
    msg <- paste0(
      "Some assessment set rows are not available at ",
      "prediction time. "
    )

    if (tune:::has_preprocessor_recipe(workflow)) {
      msg <- paste0(
        msg,
        "Consider using `skip = TRUE` on any recipe steps that remove rows ",
        "to avoid calling them on the assessment set."
      )
    } else {
      msg <- paste0(
        msg,
        "Did your preprocessing steps filter or remove rows?"
      )
    }

    rlang::abort(msg)
  }

  # Determine the type of prediction that is required
  type_info <- metrics_info(metrics)
  types <- unique(type_info$type)

  res <- NULL
  merge_vars <- c(".row", names(grid))

  for (type_iter in types) {
    # Regular predictions
    tmp_res <-
      predict(model, x_vals, type = type_iter) %>%
      mutate(.row = orig_rows) %>%
      cbind(grid, row.names = NULL) %>%
      tibble::as_tibble()

    if (!is.null(submodels)) {
      submod_length <- lengths(submodels)
      has_submodels <- any(submod_length > 0)

      if (has_submodels) {
        submod_param <- names(submodels)
        mp_call <-
          rlang::call2(
            "multi_predict",
            .ns = "parsnip",
            object = expr(model),
            new_data = expr(x_vals),
            type = "raw",
            !!!make_submod_arg(grid, model, submodels)
          )
        tmp_res <-
          rlang::eval_tidy(mp_call) %>%
          mutate(.row = orig_rows) %>%
          unnest(cols = dplyr::starts_with(".pred")) %>%
          cbind(dplyr::select(grid, -dplyr::all_of(submod_param)), row.names = NULL) %>%
          tibble::as_tibble() %>%
          # go back to user-defined name
          dplyr::rename(!!!make_rename_arg(grid, model, submodels)) %>%
          dplyr::select(dplyr::one_of(names(tmp_res))) %>%
          dplyr::bind_rows(tmp_res)
      }
    }

    if (!is.null(res)) {
      res <- dplyr::full_join(res, tmp_res, by = merge_vars)
    } else {
      res <- tmp_res
    }

    rm(tmp_res)
  } # end type loop

  # Add outcome data
  y_vals <- dplyr::mutate(y_vals, .row = orig_rows)
  res <- dplyr::full_join(res, y_vals, by = ".row")

  # # Add case weights (if needed)
  # if (tune::has_case_weights(workflow)) {
  #   case_weights <- tune::extract_case_weights(new_data, workflow)

  #   if (tune::.use_case_weights_with_yardstick(case_weights)) {
  #     case_weights <- rlang::list2(!!case_weights_column_name() := case_weights)
  #     case_weights <- vctrs::new_data_frame(case_weights)
  #     case_weights <- dplyr::mutate(case_weights, .row = orig_rows)
  #     res <- dplyr::full_join(res, case_weights, by = ".row")
  #   }
  # }
  tibble::as_tibble(res)
}

make_submod_arg <- function(grid, model, submodels) {
  # Assumes only one submodel parameter per model
  ## all possible submodels, but not all actually used
  real_name <-
    parsnip::get_from_env(paste(class(model$spec)[1], "args", sep = "_")) %>%
    dplyr::filter(has_submodel & engine == model$spec$engine) %>%
    dplyr::pull(parsnip)

  # cli::cli_inform("real_name: {real_name}")
  # cli::cli_inform("names(submodels): {names(submodels)}")
  # cli::cli_inform("submodels: {submodels}")

  # FIXME: here I'll comment this line, but this need to be checked when the names are different
  # from parsnip and the original model pkg
  # names(submodels) <- real_name
  submodels
}

make_rename_arg <- function(grid, model, submodels) {
  # Assumes only one submodel parameter per model
  ## all possible submodels, but not all actually used
  real_name <-
    parsnip::get_from_env(paste(class(model$spec)[1], "args", sep = "_")) %>%
    dplyr::filter(has_submodel & engine == model$spec$engine) %>%
    dplyr::pull(parsnip)

  # FIXME: his need to be checked when the names are different
  # from parsnip and the original model pkg

  res <- split(real_name, real_name) # list(real_name)   # this allows multiple parameters
  res <- purrr::keep(res, names(res) %in% names(submodels)) # this hack keeps only the ones that are in the submodels
  names(res) <- names(submodels)
  res
}
mytune <- rlang::ns_env("tune")
unlockBinding(rlang::sym("make_submod_arg"), mytune)
unlockBinding(rlang::sym("make_rename_arg"), mytune)
# unlockBinding(rlang::sym("predict_model"), mytune)
# unlockBinding(rlang::sym("tune_grid_loop_iter"), mytune)
mytune$make_rename_arg <- make_rename_arg
mytune$make_submod_arg <- make_submod_arg
# mytune$predict_model <- predict_model
# mytune$tune_grid_loop_iter <- tune_grid_loop_iter

min_grid.floss_regime_model <- function(x, grid, ...) { # nolint
  # cli::cli_inform(c("*" = "min_grid.floss_regime_model "))
  # cli::cli_inform(c("*" = "min_grid.floss_regime_model: dots_n {rlang::dots_n(...)}"))
  if (rlang::dots_n(...) > 0) { # 0
    cli::cli_alert(c("*" = "min_grid.floss_regime_model: dots_names {names(rlang::dots_list(..., .preserve_empty = TRUE))}"))
  }

  submod_and_othersx <- function(grid, fixed_args) {
    orig_names <- names(grid)
    subm_nm <- orig_names[!(orig_names %in% fixed_args)]
    grid <- grid %>% dplyr::rename(..val = !!subm_nm)
    fit_only <- grid %>%
      dplyr::group_by(!!!rlang::syms(fixed_args)) %>%
      dplyr::summarize(max_val1 = max(..val1, na.rm = TRUE), max_val2 = max(..val2, na.rm = TRUE)) %>%
      dplyr::ungroup()

    min_grid_df <- dplyr::full_join(fit_only, grid, by = fixed_args) %>%
      dplyr::filter(..val1 != max_val1, ..val2 != max_val2) %>%
      dplyr::group_by(!!!rlang::syms(fixed_args)) %>%
      dplyr::summarize(.submodels = list(tibble::lst(!!subm_nm[1] := ..val1, !!subm_nm[2] := ..val2))) %>%
      dplyr::ungroup() %>%
      dplyr::full_join(fit_only,
        by = fixed_args
      ) %>%
      dplyr::rename(!!subm_nm[1] := max_val1, !!subm_nm[2] := max_val2)
    min_grid_df$.submodels <- dplyr::if_else(!purrr::map_lgl(
      min_grid_df$.submodels,
      rlang::is_null
    ), min_grid_df$.submodels, purrr::map(
      seq_len(nrow(min_grid_df)),
      ~ list()
    ))
    dplyr::select(min_grid_df, dplyr::one_of(orig_names), .submodels) %>%
      dplyr::mutate_if(is.factor, as.character)
  }

  submod_and_others1 <- function(grid, fixed_args) {
    orig_names <- names(grid)
    subm_nm <- orig_names[!(orig_names %in% fixed_args)]
    grid <- grid %>% dplyr::rename(..val = !!subm_nm)
    fit_only <- grid %>%
      dplyr::group_by(!!!rlang::syms(fixed_args)) %>%
      dplyr::summarize(max_val = max(..val, na.rm = TRUE)) %>%
      dplyr::ungroup()

    min_grid_df <- dplyr::full_join(fit_only, grid, by = fixed_args) %>%
      dplyr::filter(..val != max_val) %>%
      dplyr::group_by(!!!rlang::syms(fixed_args)) %>%
      dplyr::summarize(.submodels = list(tibble::lst(!!subm_nm[1] := ..val))) %>%
      dplyr::ungroup() %>%
      dplyr::full_join(fit_only,
        by = fixed_args
      ) %>%
      dplyr::rename(!!subm_nm[1] := max_val)
    min_grid_df$.submodels <- dplyr::if_else(!purrr::map_lgl(
      min_grid_df$.submodels,
      rlang::is_null
    ), min_grid_df$.submodels, purrr::map(
      seq_len(nrow(min_grid_df)),
      ~ list()
    ))
    dplyr::select(min_grid_df, dplyr::one_of(orig_names), .submodels) %>%
      dplyr::mutate_if(is.factor, as.character)
  }

  gr_nms <- names(grid)
  param_info <- tune:::get_submodel_info(x)
  sub_nm <- param_info$id[param_info$has_submodel]
  if (length(sub_nm) == 0 | !any(names(grid) %in% sub_nm)) {
    return(tune:::blank_submodels(grid))
  }
  fixed_args <- gr_nms[!(gr_nms %in% sub_nm)]
  if (length(fixed_args) == 0) {
    res <- tune:::submod_only(grid)
  } else {
    res <- submod_and_others1(grid, fixed_args)
  }
  res

  # # This is basically `fit_max_value()` with an extra error trap
  # gr_nms <- names(grid)
  # param_info <- tune:::get_submodel_info(x)
  # sub_nm <- param_info$id[param_info$has_submodel]

  # if (length(sub_nm) == 0) {
  #   return(tune:::blank_submodels(grid))
  # }

  # fixed_args <- gr_nms[gr_nms != sub_nm]

  # if (length(fixed_args) == 0) {
  #   res <- tune:::submod_only(grid)
  # } else {
  #   res <- tune:::submod_and_others(grid, fixed_args)
  # }
  # res
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

# Stack:

# training:
# min_grid.floss_regime_model
# check_args.floss_regime_model
# translate.floss_regime_model
# > .check_floss_regime_threshold_fit
# train_regime_model

# predict:
# predict.floss_regime_model
# multi_predict._floss_regime_model
# > predict.floss_regime_model

# evaluate:
# floss_error
# floss_error.data.frame
# floss_error_vec


# #' Organize FLOSS predictions
# #'
# #' This function is for developer use and organizes predictions from floss
# #' models.
# #'
# #' @param x Predictions as returned by the `predict()` method for floss models.
# #' @param object An object of class `model_fit`.
# #'
# #' @rdname floss_helpers_prediction
# #' @keywords internal
# #' @export
# .organize_floss_regime_pred <- function(x, object) {
#   # cli::cli_inform(c("*" = ".organize_floss_regime_pred"))

#   n <- nrow(x)

#   if (!is.null(object$spec$args$regime_threshold)) {
#     regime_threshold <- rep(object$spec$args$regime_threshold, each = n)
#   } else {
#     regime_threshold <- rep(object$fit$terms$regime_threshold, each = n)
#   }

#   res <- dplyr::bind_cols(id = seq_len(n), regime_threshold = regime_threshold, x)
#   res <- res %>%
#     nest(.pred = !id) %>%
#     select(!id)
#   res
# }

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

# #' @param regime_threshold A regime_threshold value to check.
# #' @param object An object of class `model_fit`.
# #' @param multi A logical indicating if multiple values are allowed.
# #'
# #' @rdname floss_helpers
# #' @keywords internal
# #' @export
# .check_floss_regime_threshold_predict <- function(regime_threshold = NULL, object, multi = FALSE) { # nolint
#   if (is.null(regime_threshold)) {
#     regime_threshold <- object$fit$terms$regime_threshold
#   }

#   if (length(object$fit$terms$window_size) > 1) {
#     rlang::abort("window_size must be a single value")
#   }
#   if (length(object$fit$terms$mp_threshold) > 1) {
#     rlang::abort("mp_threshold must be a single value")
#   }
#   if (length(object$fit$terms$time_constraint) > 1) {
#     rlang::abort("time_constraint must be a single value")
#   }

#   # when using `predict()`, allow for a single regime_threshold
#   if (!multi) {
#     if (length(regime_threshold) != 1) {
#       rlang::abort(
#         glue::glue(
#           "`regime_threshold` should be a single numeric value. `multi_predict()` ",
#           "can be used to get multiple predictions per row of data.",
#         )
#       )
#     }
#   }

#   regime_threshold
# }

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
  func = list(fun = "regime_threshold_par"),
  has_submodel = TRUE
)

parsnip::set_model_arg(
  model = "floss_regime_model",
  eng = "floss",
  parsnip = "regime_landmark",
  original = "regime_landmark",
  func = list(fun = "regime_landmark_par"),
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
