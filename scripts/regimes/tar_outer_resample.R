library(tidymodels)
options(tidymodels.dark = TRUE)
##################################### Testing #####################################
source(here("scripts/regimes", "parsnip_model.R"))
tar_load(analysis_split)
class(analysis_split) <- c("manual_rset", "rset", class(analysis_split))
the_data <- analysis_split$splits[[1]]$data

# floss_spec <-
#   floss_regime_model(
#     window_size = tune(),
#     time_constraint = tune(),
#     mp_threshold = tune(),
#     regime_threshold = tune(),
#     regime_landmark = tune()
#   ) %>%
#   parsnip::set_engine("floss") %>%
#   parsnip::set_mode("regression")

floss_spec <-
  floss_regime_model(
    window_size = tune(),
    time_constraint = tune(),
    mp_threshold = tune(),
    regime_threshold = tune(),
    regime_landmark = 3
  ) %>%
  parsnip::set_engine("floss") %>%
  parsnip::set_mode("regression")

floss_set <- tune::extract_parameter_set_dials(floss_spec)
# floss_set <- floss_set %>% stats::update(
#   # window_size = window_size_par(c(100, 150)),
#   # mp_threshold = mp_threshold_par(c(0.3, 0.6)),
#   # time_constraint = time_constraint_par(c(700L, 900L)),
#   regime_threshold = regime_threshold_par(c(0.1, 0.9), trans_round(0.1)),
#   regime_landmark = regime_landmark_par(c(3, 6), trans_round(0.5))
# )

floss_rec <- recipes::recipe(x = head(analysis_split$splits[[1]]$data, 1)) %>%
  recipes::update_role(truth, new_role = "outcome") %>%
  recipes::update_role(id, new_role = "predictor") %>%
  recipes::update_role(ts, new_role = "predictor")

# formula(prep(floss_rec))
# truth ~ id + ts

# doParallel::registerDoParallel(cores = 4)
#
floss_wflow <-
  workflows::workflow() %>%
  workflows::add_model(floss_spec) %>%
  workflows::add_recipe(floss_rec)

# fitted_wflow <- floss_wflow %>% parsnip::fit(the_data)
# sp <- fitted_wflow$fit$fit %>% predict(new_data = the_data)
# mp <- fitted_wflow$fit$fit %>% multi_predict(new_data = the_data, regime_threshold = c(0.1, 0.2, 0.3, 0.4), regime_landmark = c(3, 4, 5, 6))

# has_multi_predict(fitted_wflow$fit$fit) # TRUEls()sss
# has_multi_predict(floss_spec) # FALSE
# multi_predict_args(fitted_wflow$fit$fit) # regime_threshold

# options(vsc.dev.args = list(width = 1000, height = 500))


# doParallel::registerDoParallel(cores = 4)

control_parsnip(verbosity = 1L)
trade_off_decay <- function(iter) {
  expo_decay(iter, start_val = .01, limit_val = 0, slope = 1 / 4)
}
start_time <- Sys.time()
floss_search_res <- floss_spec %>%
  # tune::fit_resamples(
  #   preprocessor = floss_rec,
  #   resamples = analysis_split,

  #   metrics = yardstick::metric_set(floss_error_macro)
  # )
  tune::tune_grid( # 100 54s
    preprocessor = floss_rec,
    resamples = analysis_split,
    param_info = floss_set,
    grid = 200,
    metrics = yardstick::metric_set(floss_error_macro),
    control = tune::control_grid(
      verbose = FALSE,
      allow_par = FALSE,
      # save_workflow = TRUE,
      save_pred = TRUE,
      parallel_over = "resamples"
    )
  )





# m <- collect_metrics(floss_search_res, summarize = T)
# tune::tune_bayes( # 48 5.633692 mins
#   preprocessor = floss_rec,
#   resamples = analysis_split,
#   param_info = floss_set,
#   initial = 10,
#   iter = 10,
#   metrics = yardstick::metric_set(floss_error_micro, floss_error_macro),
#   objective = tune::exp_improve(trade_off_decay),
#   control = tune::control_bayes(
#     no_improve = 50,
#     verbose = TRUE,
#     save_pred = TRUE,
#     parallel_over = "everything"
#   )
# )

end_time <- Sys.time()
end_time - start_time

# finetune::tune_race_win_loss( # 100; 52 sec
#           preprocessor = floss_rec,
#           resamples = analysis_split,
#           param_info = floss_set,
#           grid = 100,
#           metrics = yardstick::metric_set(floss_error),
#           control = finetune::control_race(
#             verbose_elim = TRUE,
#             verbose = TRUE,
#             allow_par = FALSE,
#             save_pred = TRUE,
#             parallel_over = "everything"
#           )
#         )
# finetune::tune_race_anova( # 100; 43 sec
#             preprocessor = floss_rec,
#             resamples = analysis_split,
#             param_info = floss_set,
#             grid = 100,
#             metrics = yardstick::metric_set(floss_error),
#             control = finetune::control_race(
#               verbose_elim = TRUE,
#               verbose = TRUE,
#               allow_par = TRUE,
#               save_pred = TRUE,
#               parallel_over = "resamples"
#             )
#           )

# finetune::tune_sim_anneal( # 46 models, 26min
#   preprocessor = floss_rec,
#   resamples = analysis_split,
#   iter = 50,
#   initial = 10,
#   param_info = floss_set,

#   metrics = yardstick::metric_set(floss_error),
#   control = finetune::control_sim_anneal(
#     verbose = TRUE,
#     save_pred = TRUE,
#     no_improve = 10,
#     parallel_over = "resamples"
#   )
# )





#############
#############
#############

#############
#############
# tar_load(analysis_split)
# class(analysis_split) <- c("manual_rset", "rset", class(analysis_split))
# the_data <- analysis_split$splits[[1]]$data

# floss_spec <-
#   floss_regime_model(
#     window_size = 200,
#     time_constraint = 1250, # tune(),
#     mp_threshold = 0.5, # tune(),
#     regime_threshold = 0.2
#   ) %>%
#   parsnip::set_engine("floss") %>%
#   parsnip::set_mode("regression")

# floss_rec <- recipes::recipe(x = head(analysis_split$splits[[1]]$data, 1)) %>%
#   recipes::update_role(truth, new_role = "outcome") %>%
#   recipes::update_role(id, new_role = "predictor") %>%
#   recipes::update_role(ts, new_role = "predictor")

# floss_wflow <-
#   workflows::workflow() %>%
#   workflows::add_model(floss_spec) %>%
#   workflows::add_recipe(floss_rec)

# floss_fit <- floss_wflow %>% fit(data = the_data)

# a <- floss_fit %>% predict(the_data, regime_threshold = 0.1)

#### lin fit
#### lin fit
#### lin fit
#### lin fit
#### lin fit

# lin_spec <- linear_reg(penalty = tune()) %>%
#   parsnip::set_engine("glmnet") %>%
#   parsnip::set_mode("regression")

# lin_set <- tune::extract_parameter_set_dials(lin_spec)
# # lin_set <- lin_set %>% stats::update(
# #   # window_size = window_size_par(c(99, 100)),
# #   # mp_threshold = mp_threshold_par(c(0, 0.9), trans = multidiscr(1)),
# #   # time_constraint = time_constraint_par(c(750L, 2500L)),
# #   penalty = dials::penalty(c(0.1, 0.4, 0.6, 0.9))
# # )


# lin_rec <- recipes::recipe(x = head(mtcars, 1)) %>%
#   recipes::update_role(mpg, new_role = "outcome") %>%
#   recipes::update_role(cyl, new_role = "predictor") %>%
#   recipes::update_role(disp, new_role = "predictor") %>%
#   recipes::update_role(hp, new_role = "predictor") %>%
#   recipes::update_role(drat, new_role = "predictor") %>%
#   recipes::update_role(wt, new_role = "predictor") %>%
#   recipes::update_role(qsec, new_role = "predictor") %>%
#   recipes::update_role(vs, new_role = "predictor") %>%
#   recipes::update_role(am, new_role = "predictor") %>%
#   recipes::update_role(gear, new_role = "predictor") %>%
#   recipes::update_role(carb, new_role = "predictor")

# lin_wflow <-
#   workflows::workflow() %>%
#   workflows::add_model(lin_spec) %>%
#   workflows::add_recipe(lin_rec)
# lin_fit <- lin_wflow %>% fit(data = mtcars)

# lin_fit$fit$fit %>% predict(mtcars[, -1], penalty = 0.1)
# lin_fit$fit$fit %>% multi_predict(mtcars[, -1], penalty = c(0.1, 0.5, 0.9))
# a$predict_model <- predict_model

# lin_spec_res <- lin_spec %>%
#   tune::tune_grid(
#     preprocessor = lin_rec,
#     resamples = apparent(mtcars),
#     param_info = lin_set,
#     grid = 30,
#     # metrics = yardstick::metric_set(floss_error),
#     control = tune::control_grid(verbose = TRUE, allow_par = FALSE, parallel_over = "resamples")
#   )

# A tibble: 32 × 1
#    .pred
#    <list>
#  1 <tibble [3 × 2]>
#  2 <tibble [3 × 2]>


# predicted <- fitted_mod %>%
#   predict(the_data) %>%
#   dplyr::bind_cols(the_data)

# predicted$truth[[13]]
# predicted$.pred[[13]]

# eval <- floss_error(predicted, truth = predicted$truth, estimate = predicted$.pred, estimator = "macro")
# eval

# str(predicted$.pred, 1)

# wflw_set <- workflow_set(preproc = list(simple = floss_rec), models = list(floss = floss_spec), cross = FALSE)

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

# library(ggforce)

# set.seed(1)
# floss_set %>%
#   dials::grid_max_entropy(size = 20, original = F) %>%
#   ggplot(aes(x = .panel_x, y = .panel_y)) +
#   geom_point() +
#   geom_blank() +
#   facet_matrix(vars(window_size, mp_threshold, time_constraint, regime_threshold)) +
#   labs(title = "Latin Hypercube design with 20 candidates")


# library(doMC)
# doMC::registerDoMC(cores = parallel::detectCores())


# param_key <- tibble(group = colnames(a), regime_threshold = c(0.3, 0.6))
# pred$.row <- seq_len(nrow(pred))
# pred <- gather(pred, group, .pred, -.row)
# pred <- full_join(param_key, pred, by = "group")
# pred$group <- NULL
# pred <- arrange(pred, .row, regime_threshold)
# .row <- pred$.row
# pred$.row <- NULL
# pred <- split(pred, .row)
# names(pred) <- NULL


# library(tidymodels)
# data(cells)
# cells <- cells %>% select(-case)

# set.seed(1304)
# cell_folds <- vfold_cv(cells, v = 4)
# unifold <- initial_split(cells)

# c5_spec <-
#   boost_tree(trees = 5) %>%
#   set_engine("C5.0") %>%
#   set_mode("classification")

# roc_res <- metric_set(roc_auc)
# doParallel::registerDoParallel(cores = 4)
# control_parsnip(verbosity = 2L)
# set.seed(1307)
# c5_spec %>%
#   tune_grid(
#     class ~ .,
#     resamples = cell_folds,
#     grid = data.frame(trees = 1:100),
#     metrics = roc_res,
#     control = tune::control_grid(verbose = TRUE, allow_par = TRUE, parallel_over = "resamples")
#   )



# https://finetune.tidymodels.org/ (simulated anealing)
# finetune::tune_race_anova(
#   preprocessor = floss_rec,
#   resamples = analysis_split,
#   # To use non-default parameter ranges
#   param_info = floss_set,
#   grid = 30,
#   # How to measure performance?
#   metrics = yardstick::metric_set(floss_error),
#   control = finetune::control_race(verbose_elim = TRUE)
# )
# tune_race_anova

# tune::tune_bayes(
#   preprocessor = floss_rec,
#   resamples = analysis_split,
#   # To use non-default parameter ranges
#   param_info = floss_set,
#   # Generate at semi-random to start
#   initial = 10,
#   iter = 10,
#   # How to measure performance?
#   metrics = yardstick::metric_set(floss_error), # help has the function signature
#   control = tune::control_bayes(no_improve = 10, verbose = TRUE, save_pred = TRUE, parallel_over = "resamples")
# )

# doParallel::stopImplicitCluster()

# estimates <-
#   collect_metrics(floss_search_res) %>%
#   arrange(.iter)

# estimates

# show_best(floss_search_res, metric = "floss_error")
# autoplot(floss_search_res, type = "performance")
# autoplot(floss_search_res, type = "parameters") +
#   labs(x = "Iterations", y = NULL)

# best_model <- select_best(floss_search_res, metric = "floss_error")

# final_wflow <-
#   floss_wflow %>%
#   finalize_workflow(best_model)

# final_fit <-
#   final_wflow %>%
#   last_fit(cell_split)

# final_model <- extract_workflow(final_fit)

# final_fit %>%
#   collect_metrics()

# final_model %>%
#   extract_fit_engine() %>%
#   rpart.plot(roundint = FALSE)

# library(vip)

# final_model %>%
#   flofit() %>%
#   extract_fit_parsnip() %>%
#   vip()

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


# library(kknn)
# knn_spec <- nearest_neighbor(mode = "regression", neighbors = 5) %>%
#   set_engine("kknn")
# knn_fit <- knn_spec  %>% fit(mpg ~ ., mtcars)

# has_multi_predict(knn_fit) # TRUE
# has_multi_predict(knn_spec) # FALSE
# multi_predict_args(knn_fit) # neighbors
# multi_predict(knn_fit, mtcars[1, -1], neighbors = 1:4)$.pred
