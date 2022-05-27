parsnip::set_new_model("floss_regime_model")

parsnip::set_model_mode(model = "floss_regime_model", mode = "regression")

# ------------------------------------------------------------------------------

parsnip::set_model_engine(
  "floss_regime_model",
  mode = "regression",
  eng = "floss"
)

parsnip::set_dependency("floss_regime_model", eng = "floss")


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
