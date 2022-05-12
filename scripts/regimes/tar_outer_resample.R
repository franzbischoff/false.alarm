source(here("scripts/helpers", "parsnip_model.R"))

##################################### Testing #####################################

tar_load(tidy_dataset)
df_test <- data.frame(y = as.numeric(tsmp::mp_toy_data$data[, 1]), x = as.numeric(tsmp::mp_toy_data$data[, 2])) # , z = as.numeric(tsmp::mp_toy_data$data[, 3]))

floss_mod <-
  floss_regime_model(
    window_size = tune(),
    time_constraint = tune(),
    mp_threshold = tune(),
    regime_threshold = tune()
  ) %>%
  parsnip::set_engine("floss") %>%
  parsnip::set_mode("regression")

# floss_mod <-
#   floss_regime_model(
#     window_size = 200,
#     time_constraint = 1250,
#     mp_threshold = 0,
#     regime_threshold = 0.3
#   ) %>%
#   parsnip::set_engine("floss") %>%
#   parsnip::set_mode("regression")

# flofit <- floss_mod %>% fit(y ~ x, data = tidy_dataset)


# summary(floss_rec)

floss_set <- hardhat::extract_parameter_set_dials(floss_mod)
floss_set <- floss_set %>% stats::update(
  window_size = window_size_par(c(150L, 350L)),
  mp_threshold = mp_threshold_par(c(0, 0.9)),
  time_constraint = time_constraint_par(c(0L, 2500L)),
  regime_threshold = dials::threshold(c(0.2, 0.6))
)

set.seed(456)
folds <- vfold_cv(tidy_dataset, v = 2, repeats = 5)
folds <- apparent(tidy_dataset) # train and test are the same
fold <- folds[1, ]
folds <- manual_rset(list(
  fold$splits[[1]],
  fold$splits[[1]],
  fold$splits[[1]],
  fold$splits[[1]],
  fold$splits[[1]],
  fold$splits[[1]]
), ids = as.character(1:6))

for (i in seq.int(1, length(folds$splits))) {
  folds$splits[[i]]$in_id <- sample(1:5)
  folds$splits[[i]]$out_id <- sample(6:10)
  class(folds$splits[[i]]) <- c("mc_split", "rsplit")
}

# split <- initial_split(df_test, prop = 3 / 4)

floss_rec <- recipes::recipe(x = tidy_dataset) %>%
  recipes::update_role(truth, new_role = "outcome") %>%
  recipes::update_role(id, new_role = "predictor") %>%
  recipes::update_role(ts, new_role = "predictor")


# doParallel::registerDoParallel(cores = 4)
#
# floss_wflow <-
#   workflow() %>%
#   add_model(floss_mod) %>%
#   add_recipe(floss_rec)


# wflw_set <- workflow_set(preproc = list(simple = floss_rec), models = list(floss = floss_mod), cross = FALSE)

# nobel_models <- wflw_set %>%
#   workflow_map("tune_grid", # função do pacote {{tune}}
#     resamples = folds,
#     param_info = floss_set,
#     grid = 30,
#     metrics = metric_set(floss_error), # recall, spec, precision
#     verbose = TRUE
#   )


# wflw_set %>%
#   workflow_map(tune::tune_bayes(
#     # preprocessor = floss_rec,
#     resamples = folds,
#     # To use non-default parameter ranges
#     param_info = floss_set,
#     # Generate five at semi-random to start
#     initial = 5,
#     iter = 20,
#     # How to measure performance?
#     metrics = yardstick::metric_set(floss_error), # help has the function signature
#     control = tune::control_bayes(no_improve = 20, verbose = TRUE, save_pred = TRUE, parallel_over = "everything")
#   ))


# library(doMC)
# doMC::registerDoMC(cores = parallel::detectCores())

floss_search_res <- floss_mod %>%
  # tune::fit_resamples(
  #   preprocessor = floss_rec,
  #   resamples = folds,
  #   metrics = yardstick::metric_set(floss_error)
  # )
  # tune::tune_grid(
  #   preprocessor = floss_rec,
  #   resamples = folds,
  #   param_info = floss_set,
  #   grid = 30,
  #   metrics = yardstick::metric_set(floss_error),
  #   control = tune::control_grid(verbose = TRUE, allow_par = TRUE, save_pred = TRUE)
  # )
  tune::tune_bayes(
    preprocessor = floss_rec,
    resamples = folds,
    # To use non-default parameter ranges
    param_info = floss_set,
    # Generate five at semi-random to start
    initial = 6,
    iter = 10,
    # How to measure performance?
    metrics = yardstick::metric_set(floss_error), # help has the function signature
    control = tune::control_bayes(no_improve = 20, verbose = TRUE, save_pred = TRUE, parallel_over = "resamples")
  )

# doParallel::stopImplicitCluster()

estimates <-
  collect_metrics(floss_search_res) %>%
  arrange(.iter)

estimates

show_best(floss_search_res, metric = "floss_error")
autoplot(floss_search_res, type = "performance")
autoplot(floss_search_res, type = "parameters") +
  labs(x = "Iterations", y = NULL)

best_model <- select_best(floss_search_res, metric = "floss_error")

final_wflow <-
  floss_wflow %>%
  finalize_workflow(best_model)

final_fit <-
  final_wflow %>%
  last_fit(cell_split)

final_model <- extract_workflow(final_fit)

final_fit %>%
  collect_metrics()

final_model %>%
  extract_fit_engine() %>%
  rpart.plot(roundint = FALSE)

library(vip)

final_model %>%
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
