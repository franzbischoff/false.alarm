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

multidiscr <- function(mult = 10) {
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

train_regime_model <- function(..., window_size, verbose) {
  other_args <- list(...)
  rlang::inform("teste")
  browser()
  # n <- nrow(x)
  # if (n == 0) {
  #   rlang::abort("There are zero rows in the predictor set.")
  # }
  # matrixprofiler::mov_mean(x[[1]], window_size)
  # eval_tidy(fit_call)
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

parsnip::set_fit(
  model = "floss_regime_model",
  eng = "floss",
  mode = "regression",
  value = list(
    interface = "data.frame",
    data = c(x = "x.train", y = "y.gtruth"),
    protect = c("x", "y"),
    func = c(fun = "train_regime_model"),
    defaults = list(verbose = FALSE)
  )
)

### Prediction parameters

# parsnip::set_pred(
#   model = "floss_regime_model",
#   eng = "floss",
#   mode = "regression",
#   type = "numeric",
#   value = list(
#     pre = NULL,
#     post = NULL,
#     func = c(fun = "predict"),
#     args =
#     # These lists should be of the form:
#     # {predict.mda argument name} = {values provided from parsnip objects}
#       list(
#         # We don't want the first two arguments evaluated right now
#         # since they don't exist yet. `type` is a simple object that
#         # doesn't need to have its evaluation deferred.
#         object = rlang::expr(object$fit),
#         newdata = rlang::expr(new_data),
#         type = "numeric"
#       )
#   )
# )

# parsnip::set_pred(
#   model = "floss_regime_model",
#   eng = "floss",
#   mode = "regression",
#   type = "prob",
#   value = parsnip::pred_value_template(
#     post = function(x, object) {
#       tibble::as_tibble(x)
#     },
#     func = c(fun = "predict"),
#     # Now everything else is put into the `args` slot
#     object = rlang::expr(object$fit),
#     newdata = rlang::expr(new_data),
#     type = "posterior"
#   )
# )

## Yardstick custom metric

floss_error <- function(data, ...) {
  UseMethod("floss_error")
}

floss_error <- yardstick::new_numeric_metric(floss_error, direction = "minimize")

floss_error.data.frame <- function(data, truth, estimate, data_size, na_rm = TRUE, ...) { # nolint
  yardstick::metric_summarizer(
    metric_nm = "floss_error",
    metric_fn = floss_error_vec,
    data = data,
    truth = !!rlang::enquo(truth),
    estimate = !!rlang::enquo(estimate),
    na_rm = na_rm,
    ...,
    metric_fn_options = list(data_size = data_size)
  )
}

floss_error_vec <- function(truth, estimate, data_size, na_rm = TRUE, ...) {
  floss_error_impl <- function(truth, estimate, data_size) {
    score_regimes(truth, estimate, data_size)
  }

  yardstick::metric_vec_template(
    metric_impl = floss_error_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    data_size = data_size,
    ...
  )
}


##################################### Testing #####################################

df_test <- data.frame(data = as.numeric(tsmp::mp_toy_data$data[, 1]), other = as.numeric(tsmp::mp_toy_data$data[, 2]), third = as.numeric(tsmp::mp_toy_data$data[, 3])) # , y = tsmp::mp_toy_data$data[, 2])

# parsnip::show_model_info("floss_regime_model")

floss_regime_model(window_size = 100) %>% translate(engine = "floss")

# floss_spec <- floss_regime_model(window_size = 100) %>%
#   parsnip::set_engine("floss")

# floss_fit <- workflow() %>%
#   add_variables(outcome = y, predictors = x) %>%
#   add_model(floss_spec, formula = y ~ x)

# test <- floss_regime_model(window_size = 100) %>%
#   parsnip::set_engine("floss") %>%
#   parsnip::fit(data ~ ., data = df_test)
# floss_fit, data = data.frame(x = tsmp::mp_toy_data$data[, 1], y = tsmp::mp_toy_data$data[, 2]))

floss_mod <-
  floss_regime_model(
    window_size = tune(),
    time_constraint = tune(),
    mp_threshold = tune(),
    regime_threshold = tune()
  ) %>%
  parsnip::set_engine("floss") %>%
  parsnip::set_mode("regression")

floss_wflow <-
  workflow() %>%
  add_model(floss_mod) %>%
  add_formula(data ~ .)

floss_set <- hardhat::extract_parameter_set_dials(floss_wflow)
floss_set <- floss_set %>% stats::update(
  window_size = window_size_par(c(150L, 350L)),
  mp_threshold = mp_threshold_par(c(0, 0.9)),
  time_constraint = time_constraint_par(c(0L, 2500L)),
  regime_threshold = dials::threshold(c(0.2, 0.6))
)

set.seed(456)
folds <- vfold_cv(df_test, v = 10)

floss_search_res <- floss_wflow %>%
  tune::tune_bayes(
    resamples = folds,
    # To use non-default parameter ranges
    param_info = floss_set,
    seed = sample.int(10^5, 1),
    # Generate five at semi-random to start
    initial = 5,
    iter = 50,
    # How to measure performance?
    metrics = yardstick::metric_set(floss_error), # help has the function signature
    control = tune::control_bayes(no_improve = 30, verbose = TRUE, save_pred = TRUE)
  )

estimates <-
  collect_metrics(floss_search_res) %>%
  arrange(.iter)

estimates

show_best(floss_search_res, metric = "rmse")
autoplot(floss_search_res, type = "performance")
autoplot(floss_search_res, type = "parameters") +
  labs(x = "Iterations", y = NULL)

set.seed(456)
folds <- vfold_cv(df_test, v = 10)
set.seed(456)
floss_fit_rs <-
  floss_wflow %>%
  tune_grid(
    val_set,
    resamples = folds,
    grid = 25,
    control = control_grid(save_pred = TRUE, verbose = TRUE),
    metrics = metric_set(floss_error)
  )

collect_metrics(floss_fit_rs)

floss_fit_rs %>%
  show_best("accuracy")

best_tree <- floss_fit_rs %>%
  select_best("accuracy")

final_wflow <-
  floss_wflow %>%
  finalize_workflow(best_tree)

floss_testing_pred %>% # test set predictions
  roc_auc(truth = class, .pred_PS)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 roc_auc binary         0.891
floss_testing_pred %>% # test set predictions
  accuracy(truth = class, .pred_class)
#> # A tibble: 1 × 3
#>   .metric  .estimator .estimate
#>   <chr>    <chr>          <dbl>
#> 1 accuracy binary         0.816

# The last fit

final_fit <-
  final_wflow %>%
  last_fit(cell_split)

final_fit %>%
  collect_metrics()
#> # A tibble: 2 × 4
#>   .metric  .estimator .estimate .config
#>   <chr>    <chr>          <dbl> <chr>
#> 1 accuracy binary         0.802 Preprocessor1_Model1
#> 2 roc_auc  binary         0.840 Preprocessor1_Model1

final_tree <- extract_workflow(final_fit)
final_tree

final_fit %>%
  collect_predictions() %>%
  roc_curve(class, .pred_PS) %>%
  autoplot()

final_tree %>%
  extract_fit_engine() %>%
  rpart.plot(roundint = FALSE)

library(vip)

final_tree %>%
  extract_fit_parsnip() %>%
  vip()

# > str(test$spec)
# List of 5
#  $ args    :List of 1
#   ..$ window_size: language ~100
#   .. ..- attr(*, ".Environment")=<environment: R_EmptyEnv>
#  $ eng_args: Named list()
#   ..- attr(*, "class")= chr [1:2] "quosures" "list"
#  $ mode    : chr "regression"
#  $ method  :List of 3
#   ..$ libs: chr "parsnip"
#   ..$ fit :List of 5
#   .. ..$ interface: chr "data.frame"
#   .. ..$ protect  : chr "x"
#   .. ..$ func     : Named chr "train_regime_model"
#   .. .. ..- attr(*, "names")= chr "fun"
#   .. ..$ defaults : list()
#   .. ..$ args     :List of 2
#   .. .. ..$ x          : language missing_arg()
#   .. .. ..$ window_size: language ~100
#   .. .. .. ..- attr(*, ".Environment")=<environment: R_EmptyEnv>
#   ..$ pred: Named list()
#  $ engine  : chr "floss"
#  - attr(*, "class")= chr [1:2] "floss_regime_model" "model_spec"

# > str(test$preproc)
# List of 4
#  $ terms  :Classes 'terms', 'formula'  language data ~ other + third
#   .. ..- attr(*, "variables")= language list(data, other, third)
#   .. ..- attr(*, "factors")= int [1:3, 1:2] 0 1 0 0 0 1
#   .. .. ..- attr(*, "dimnames")=List of 2
#   .. .. .. ..$ : chr [1:3] "data" "other" "third"
#   .. .. .. ..$ : chr [1:2] "other" "third"
#   .. ..- attr(*, "term.labels")= chr [1:2] "other" "third"
#   .. ..- attr(*, "order")= int [1:2] 1 1
#   .. ..- attr(*, "intercept")= int 1
#   .. ..- attr(*, "response")= int 1
#   .. ..- attr(*, ".Environment")=<environment: 0x563707de49d8>
#   .. ..- attr(*, "predvars")= language list(data, other, third)
#   .. ..- attr(*, "dataClasses")= Named chr [1:3] "numeric" "numeric" "numeric"
#   .. .. ..- attr(*, "names")= chr [1:3] "data" "other" "third"
#  $ xlevels: Named list()
#  $ options:List of 3
#   ..$ indicators      : chr "none"
#   ..$ composition     : chr "data.frame"
#   ..$ remove_intercept: logi FALSE
#  $ y_var  : chr "data"
