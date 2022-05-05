##################################### Testing #####################################

df_test <- data.frame(y = as.numeric(tsmp::mp_toy_data$data[, 1]), x = as.numeric(tsmp::mp_toy_data$data[, 2]), z = as.numeric(tsmp::mp_toy_data$data[, 3]))

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
  add_formula(y ~ x + z)

floss_set <- hardhat::extract_parameter_set_dials(floss_wflow)
floss_set <- floss_set %>% stats::update(
  window_size = window_size_par(c(150L, 350L)),
  mp_threshold = mp_threshold_par(c(0, 0.9)),
  time_constraint = time_constraint_par(c(0L, 2500L)),
  regime_threshold = dials::threshold(c(0.2, 0.6))
)

set.seed(456)
folds <- vfold_cv(df_test, v = 2, repeats = 2)

floss_search_res <- floss_wflow %>%
  tune::tune_bayes(
    resamples = folds,
    # To use non-default parameter ranges
    param_info = floss_set,
    # Generate five at semi-random to start
    initial = 5,
    iter = 20,
    # How to measure performance?
    metrics = yardstick::metric_set(floss_error), # help has the function signature
    control = tune::control_bayes(no_improve = 5, verbose = TRUE, save_pred = TRUE)
  )

estimates <-
  collect_metrics(floss_search_res) %>%
  arrange(.iter)

estimates

show_best(floss_search_res, metric = "rmse")
autoplot(floss_search_res, type = "performance")
autoplot(floss_search_res, type = "parameters") +
  labs(x = "Iterations", y = NULL)

################################################################################

# parsnip::show_model_info("floss_regime_model")

# floss_regime_model() %>% translate(engine = "floss")

floss_spec <- floss_regime_model(window_size = 100) %>%
  parsnip::set_engine("floss")

floss_fit <- workflow() %>%
  add_variables(outcome = y, predictors = x) %>%
  add_model(floss_spec, formula = y ~ x)


df_test <- data.frame(y = as.numeric(tsmp::mp_toy_data$data[, 1]), x = as.numeric(tsmp::mp_toy_data$data[, 2]), z = as.numeric(tsmp::mp_toy_data$data[, 3]))
test <- floss_regime_model(
  window_size = tune(),
  time_constraint = tune(),
  mp_threshold = tune(),
  regime_threshold = tune()
) %>%
  parsnip::set_engine("floss") %>%
  parsnip::fit(~y, data = df_test)
# floss_fit, data = data.frame(x = tsmp::mp_toy_data$data[, 1], y = tsmp::mp_toy_data$data[, 2]))



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
