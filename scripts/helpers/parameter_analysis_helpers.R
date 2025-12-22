#' Parameter Analysis Helper Functions
#'
#' Functions extracted from regime_optimize_3.Rmd for BART-based parameter
#' importance analysis (FIRM, Permutation, SHAP methods)

#' Train BART Model with Cross-Validation
#'
#' @param data Tibble with features and outcome column named 'mean'
#' @param parallel Logical, use parallel processing (default: FALSE)
#' @param v Integer, number of CV folds (default: 10)
#' @param rep Integer, number of CV repetitions (default: 3)
#' @param grid Integer, grid size for tree tuning (default: 30)
#' @param train Optional pre-split training data
#' @param test Optional pre-split testing data
#' @return List with model, training_data, testing_data
train_models <- function(data, parallel = FALSE, v = 10, rep = 3, grid = 30, train = NULL, test = NULL) {
  if (is.null(train) && is.null(test)) {
    set.seed(616)
    initial_sampling <- rsample::initial_split(data, prop = 3 / 4)
    training_split <- rsample::training(initial_sampling)
    testing_split <- rsample::testing(initial_sampling)
  } else {
    training_split <- train
    testing_split <- test
  }

  set.seed(616)
  folds <- rsample::vfold_cv(training_split, v = v, repeats = rep)

  model_spec <- parsnip::bart(trees = parsnip::tune()) |>
    parsnip::set_mode("regression") |>
    parsnip::set_engine("dbarts")

  model_set <- hardhat::extract_parameter_set_dials(model_spec)

  wflw <- workflows::workflow() |>
    workflows::add_model(model_spec) |>
    workflows::add_formula(mean ~ .)

  if (parallel) {
    future::plan(future::multisession, workers = parallelly::availableCores(methods = "system"))
  } else {
    future::plan(future::sequential)
  }

  set.seed(2022)
  tune_search <- wflw |>
    tune::tune_grid(
      resamples = folds,
      param_info = model_set,
      grid = grid,
      metrics = yardstick::metric_set(yardstick::rmse, yardstick::rsq),
      control = tune::control_grid(
        verbose = TRUE,
        allow_par = parallel,
        save_workflow = FALSE,
        save_pred = TRUE,
        parallel_over = "everything"
      )
    )

  tune_best <- tune_search |> tune::select_best(metric = "rmse")
  final_flow <- wflw |> tune::finalize_workflow(tune_best)

  # Reset to sequential after tuning
  future::plan(future::sequential)

  return(list(
    model = final_flow,
    training_data = training_split, testing_data = testing_split
  ))
}

#' Check Parameter Interactions using FIRM/PDP
#'
#' @param model Fitted workflow/model object
#' @param train_data Training data tibble
#' @param features Character vector of feature names
#' @param parallel Logical, use parallel processing (default: FALSE)
#' @param seed Integer for reproducibility (default: 2022)
#' @return Tibble with Variables and Interaction strength
check_interactions <- function(model, train_data, features, parallel = FALSE, seed = 2022) {
  source(here::here("scripts", "helpers", "interactions.R"))

  interact <- suppressWarnings(vint(model$fit$fit,
    type = "regression",
    feature_names = features,
    data = train_data,
    n_jobs = ifelse(parallel, parallelly::availableCores(methods = "system"), 1),
    seed = seed
  ))

  # Sort by interaction strength
  interact <- interact |> dplyr::arrange(desc(Interaction))

  return(interact)
}

#' Generate SHAP Explanations
#'
#' @param model Fitted model object
#' @param train_data Training data (features only)
#' @param test_data Testing data (features only)
#' @param features Character vector of feature names
#' @param nsim Integer, number of Monte Carlo reps (default: 20)
#' @param parallel Logical, use parallel processing (default: FALSE)
#' @return Matrix of SHAP values
shap_explain <- function(model, train_data, test_data, features, nsim = 20, parallel = FALSE) {
  if (parallel) {
    doParallel::registerDoParallel(cores = parallelly::availableCores(methods = "system"))
  }

  set.seed(2022)
  shap <- fastshap::explain(model,
    feature_names = features,
    X = data.matrix(train_data), nsim = nsim,
    pred_wrapper = function(object, newdata) {
      pred <- predict(object, newdata)
      pred$.pred
    }, adjust = TRUE,
    newdata = data.matrix(test_data),
    .parallel = parallel
  )

  if (parallel) {
    doParallel::stopImplicitCluster()
  }

  return(shap)
}

#' Check Variable Importance using FIRM, Permutation, or SHAP
#'
#' @param model Fitted model object
#' @param train_data Training data tibble
#' @param test_data Testing data tibble
#' @param features Character vector of feature names
#' @param type Character: "firm", "permute", or "shap"
#' @param nsim Integer, number of simulations (default: 20)
#' @param parallel Logical, use parallel processing (default: FALSE)
#' @param seed Integer for reproducibility (default: 2022)
#' @return ggplot object with importance data
check_importance <- function(
    model, train_data, test_data, features, type = c("firm", "permute", "shap"),
    nsim = 20, parallel = FALSE, seed = 2022) {
  type <- match.arg(type)

  set.seed(seed)
  importances <- NULL

  if (type == "firm") {
    importances <- vip::vip(
      object = model,
      method = "firm",
      feature_names = features,
      pred.fun = function(object, newdata) {
        pred <- predict(object, newdata)
        return(pred$.pred)
      },
      type = "regression",
      parallel = parallel,
      ice = TRUE,
      train = train_data,
      mapping = aes(fill = Variable),
      aesthetics = list(color = "grey35", linewidth = 0.8)
    )
  } else if (type == "permute") {
    importances <- vip::vip(
      object = model,
      method = "permute",
      target = "mean",
      feature_names = features,
      type = "ratio",
      pred_wrapper = function(object, newdata) {
        pred <- predict(object, newdata)
        pred$.pred
      },
      nsim = nsim,
      metric = "rmse",
      parallel = parallel,
      keep = TRUE,
      geom = "boxplot",
      train = train_data,
      mapping = aes(fill = Variable),
      aesthetics = list(color = "grey35", linewidth = 0.5)
    )
    importances$layers[[1]]$data <- importances$layers[[1]]$data |>
      dplyr::filter(!grepl("int_.*", Variable))
  } else if (type == "shap") {
    importances <- vip::vip(
      object = model,
      method = "shap",
      feature_names = features,
      pred_wrapper = function(object, newdata) {
        pred <- predict(object, newdata)
        pred$.pred
      },
      nsim = nsim,
      train = as.data.frame(train_data),
      newdata = as.data.frame(test_data),
      .parallel = parallel,
      mapping = aes(fill = Variable),
      aesthetics = list(color = "grey35", linewidth = 0.8)
    )
  }

  importances$data <- importances$data |>
    dplyr::filter(!grepl("int_.*", Variable))

  return(importances)
}
