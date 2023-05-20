# more methods: https://rsample.tidymodels.org/reference/index.html#section-resampling-methods
fit_models <- function(initial_split, strata = "alarm", arguments) {
  checkmate::qassert(initial_split, "L+")
  checkmate::qassert(strata, "S1")
  checkmate::qassert(arguments, "L+")

  # Retrieve the training data
  data_train <- rsample::training(initial_split)
  # Create the resampling strategy (strata is used to keep true and false balanced)
  resampled_folds <- arguments$resampling(data_train,
    v = arguments$folds,
    repeats = arguments$repeats,
    strata = all_of(strata) # all_of is used to solve ambiguity
  )

  # Here the model skeleton is defined, with tuning placeholders
  model_specs <- # in this case a multi-layer perceptron
    arguments$model("classification",
      "nnet",
      hidden_units = tune(),
      penalty = NULL,
      dropout = 0.1,
      epochs = 100,
      activation = NULL
    )

  # model_specs <-
  #   discrim::naive_Bayes(smoothness = tune(), Laplace = 0) |>
  #   parsnip::set_engine("naivebayes") |>
  #   parsnip::set_mode("classification")

  # We can use a custom tune_grid or let the `tune` try by itself

  # tune_grid <- dials::grid_regular(dials::smoothness(),
  #   levels = 3
  # )
  tune_grid <- arguments$tune

  # This are just parameters for the `tune::tune_grid`
  ctrl <- tune::control_grid(
    verbose = TRUE,
    allow_par = TRUE,
    pkgs = NULL,
    save_workflow = TRUE # needed for next target
  )

  fitted_models <- tune::tune_grid(model_specs,
    arguments$formula,
    resamples = resampled_folds,
    param_info = NULL,
    grid = tune_grid,
    metrics = arguments$metric,
    control = ctrl
  )
  # tune::tune_bayes(alarm ~ val,
  #   resamples = resampled_folds,
  #   metrics = NULL, # https://yardstick.tidymodels.org/articles/metric-types.html
  #   control = tune::control_bayes(
  #     verbose = TRUE,
  #     save_workflow = TRUE # needed for next target
  #   )
  # )

  # return the models from the resamples
  fitted_models
}
