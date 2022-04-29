
library(tidymodels)
tidymodels_prefer(quiet = TRUE)

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
  func = list(pkg = "dials", fun = "window_size"),
  has_submodel = FALSE
)


floss_regime_model <- function(mode = "regression", window_size = NULL) {
  # Check for correct mode
  if (mode != "regression") {
    rlang::abort("`mode` should be 'regression'")
  }

  # Capture the arguments in quosures
  args <- list(window_size = rlang::enquo(window_size))

  # Save some empty slots for future parts of the specification
  parsnip::new_model_spec(
    "floss_regime_model",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = NULL
  )
}

parsnip::set_fit(
  model = "floss_regime_model",
  eng = "floss",
  mode = "regression",
  value = list(
    interface = "data.frame",
    protect = c("data"),
    # data,
    func = c(fun = "mov_mean", pkg = "matrixprofiler"), # pkg = "false.alarm", range, trans, values
    defaults = list()
  )
)

parsnip::set_encoding(
  model = "floss_regime_model",
  eng = "floss",
  mode = "regression",
  options = list(
    predictor_indicators = "traditional",
    compute_intercept = TRUE,
    remove_intercept = TRUE,
    allow_sparse_x = FALSE
  )
)

parsnip::set_pred(
  model = "floss_regime_model",
  eng = "floss",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args =
    # These lists should be of the form:
    # {predict.mda argument name} = {values provided from parsnip objects}
      list(
        # We don't want the first two arguments evaluated right now
        # since they don't exist yet. `type` is a simple object that
        # doesn't need to have its evaluation deferred.
        object = rlang::expr(object$fit),
        newdata = rlang::expr(new_data),
        type = "numeric"
      )
  )
)

parsnip::set_pred(
  model = "floss_regime_model",
  eng = "floss",
  mode = "regression",
  type = "prob",
  value = parsnip::pred_value_template(
    post = function(x, object) {
      tibble::as_tibble(x)
    },
    func = c(fun = "predict"),
    # Now everything else is put into the `args` slot
    object = rlang::expr(object$fit),
    newdata = rlang::expr(new_data),
    type = "posterior"
  )
)


parsnip::show_model_info("floss_regime_model")

floss_regime_model(window_size = 100) %>% translate(engine = "floss")

floss_spec <- floss_regime_model(window_size = 100) %>%
  parsnip::set_engine("floss")

floss_fit <- workflow() %>%
  add_variables(outcome = y, predictors = x) %>%
  add_model(floss_spec, formula = y ~ x)

parsnip::fit(floss_fit, data = data.frame(x = tsmp::mp_toy_data$data[, 1], y = tsmp::mp_toy_data$data[, 2]))
