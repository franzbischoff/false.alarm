source(here("scripts/common", "score_floss.R"))
library(tidymodels)
tidymodels_prefer(quiet = TRUE)
options(tidymodels.dark = TRUE)

## Tuning parameters

time_constraint_par <- function(range = c(0L, 2500L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(time_constraint = "Time Constraint"),
    finalize = NULL
  )
}

window_size_par <- function(range = c(200L, 250L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(window_size = "Window Size"),
    finalize = NULL
  )
}

multidiscr <- function(mult = 1) {
  force(mult)

  mult_trans <- function(x) {
    x * mult
  }

  div_trans <- function(x) {
    x / mult
  }

  scales::trans_new(
    "discrmult",
    transform = "div_trans",
    inverse = "mult_trans",
    domain = c(0, Inf)
  )
}

mp_threshold_par <- function(range = c(0, 1), trans = multidiscr()) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    default = 0.5,
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

train_regime_model <- function(truth, ts, ..., window_size, regime_threshold, mp_threshold, time_constraint, verbose = FALSE) {
  other_args <- list(...)
  # rlang::inform(glue::glue("names: {names(ts)}"))

  n <- nrow(ts)
  if (n == 0) {
    rlang::abort("There are zero rows in the predictor set.")
  }

  if (ncol(ts) > 1) {
    id <- ts[[1]]
    ts <- ts[[2]]
  } else {
    id <- seq.int(1, n)
  }

  res <- list(truth = truth, id = id, ts = ts)

  return(list(
    fitted.values = res,
    terms = c(
      window_size = window_size,
      regime_threshold = regime_threshold,
      mp_threshold = mp_threshold,
      time_constraint = time_constraint
    )
  ))
}

# Predictor interface
pred_regime_model <- function(obj, new_data, type) {
  res <- tibble::tibble(.pred = obj$fit$fitted.values$truth, .sizes = purrr::map(new_data$ts, length))
  res
}

## Register parsnip model

if (!any(parsnip::get_model_env()$models == "floss_regime_model")) {
  parsnip::set_new_model("floss_regime_model")
}
parsnip::set_model_mode(model = "floss_regime_model", mode = "regression")
parsnip::set_model_engine(
  "floss_regime_model",
  mode = "regression",
  eng = "floss"
)

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
  func = list(pkg = "dials", fun = "threshold"),
  has_submodel = FALSE
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

### Fitting parameters

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
    defaults = list(verbose = FALSE)
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
    func = c(fun = "pred_regime_model"),
    # Now everything else is put into the `args` slot
    obj = rlang::expr(object),
    new_data = rlang::expr(new_data),
    type = "numeric"
  )
)


## Yardstick custom metric

floss_error <- function(data, ...) {
  UseMethod("floss_error")
}

floss_error <- yardstick::new_numeric_metric(floss_error, direction = "minimize")

floss_error.data.frame <- function(data, truth, estimate, na_rm = TRUE, estimator = "binary", ...) { # nolint
  yardstick::metric_summarizer(
    metric_nm = "floss_error",
    metric_fn = floss_error_vec,
    data = data,
    truth = !!rlang::enquo(truth),
    estimate = !!rlang::enquo(estimate),
    na_rm = na_rm,
    estimator = estimator,
    list(...),
    metric_fn_options = list(data_size = data$.sizes) # purrr::map(data$ts, length))
  )
}

floss_error_vec <- function(truth, estimate, data_size, na_rm = TRUE, estimator = "binary", ...) {
  floss_error_impl <- function(truth, estimate, data_size, ...) {
    res <- score_regimes(truth, estimate, data_size)
    return(res)
  }

  est <- estimate %>% purrr::map(~ .x * runif(1))
  for (i in seq.int(1, length(estimate))) {
    lt <- length(truth[[i]])
    le <- length(estimate[[i]])
    if (lt > le) {
      est[[i]] <- c(est[[i]], rep(-1, lt - le))
    } else {
      truth[[i]] <- c(truth[[i]], rep(-1, le - lt))
    }
  }

  if (estimator == "micro") {
    res <- purrr::map2_dbl(
      truth, est,
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
    res <- purrr::pmap_dbl(
      list(truth, est, data_size),
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

# floss_error(tidy_dataset, truth = truth, estimate = truth, estimator = "micro")$.estimate
