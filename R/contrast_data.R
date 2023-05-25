register_contrast_model <- function() {
  if (!any(parsnip::get_model_env()$models == "contrast_model")) {
    parsnip::set_new_model("contrast_model")
    parsnip::set_model_mode(model = "contrast_model", mode = "classification")
  }

  # ------------------------------------------------------------------------------

  parsnip::set_model_engine(
    "contrast_model",
    mode = "classification",
    eng = "contrast_profile"
  )

  parsnip::set_dependency("contrast_model", eng = "contrast_profile")


  parsnip::set_fit(
    model = "contrast_model",
    eng = "contrast_profile",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(fun = "filter_best_solutions", pkg = "false.alarm"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "contrast_model",
    eng = "contrast_profile",
    mode = "classification",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )


  parsnip::set_pred(
    model = "contrast_model",
    eng = "contrast_profile",
    mode = "classification",
    type = "class",
    value = parsnip::pred_value_template(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      # Now everything else is put into the `args` slot
      object = rlang::expr(object$fit),
      newdata = rlang::expr(new_data),
      type = "class"
    )
  )

  parsnip::set_pred(
    model = "contrast_model",
    eng = "contrast_profile",
    mode = "classification",
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

  parsnip::set_model_arg(
    model = "contrast_model",
    eng = "contrast_profile",
    parsnip = "num_shapelets",
    original = "num_shapelets",
    func = list(fun = "num_shapelets_par"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "contrast_model",
    eng = "contrast_profile",
    parsnip = "redundance",
    original = "redundance",
    func = list(fun = "redundance_par"),
    has_submodel = FALSE
  )
}
