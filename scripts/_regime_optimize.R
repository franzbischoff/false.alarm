# Sys.setenv(TAR_PROJECT = "regime_optimize")

source(here::here("scripts", "_globals.R"), local = .GlobalEnv, encoding = "UTF-8") # nolint

# source(here("regimes", "tar_inner_resample.R"))
# source(here("regimes", "tar_outer_resample.R"))


options(target_ds_path = here("inst", "extdata", "afib_regimes")) # nolint
options(tidymodels.dark = TRUE) # nolint

#### Pipeline: variable definitions ----
# signal sample frequency, this is a constant
const_sample_freq <- 250
const_signals <- c("time", "I", "II")
const_classes <- c("persistent_afib", "paroxysmal_afib", "non_afib")

var_resample_from <- 200
var_resample_to <- const_sample_freq

# keep only the X filenames
# var_head <- 10
# The subset that will be keep from the dataset (seq.int(240 * const_sample_freq + 1, 300 * const_sample_freq) means the last 60 seconds)
# var_subset <- seq.int(240 * const_sample_freq + 1, 300 * const_sample_freq) # last 60 secs
var_subset <- NULL # 1:10000
var_limit_per_class <- NULL # 20

var_classes_include <- "paroxysmal_afib"
var_classes_exclude <- setdiff(const_classes, var_classes_include)

var_signals_include <- "II"
var_signals_exclude <- setdiff(const_signals, var_signals_include)



#### Targets: Define targets options ----

# use renv::install(".") to update the rcpp functions
tar_option_set(
  tidy_eval = TRUE,
  packages = c(
    "here", "glue", "dplyr", "rlang", "rsample", "tidyr", "false.alarm",
    "dials", "scales", "tibble", "parsnip", "yardstick", "purrr", "hardhat"
  ),
  format = "rds",
  garbage_collection = TRUE
)

source(here::here("scripts", "regimes", "training_regimes.R"), encoding = "UTF-8")
source(here::here("scripts", "regimes", "predict_regimes.R"), encoding = "UTF-8")


# The solutions for detecting when a model is overemphasizing the training set is using out-of-sample data.

############
# tuning variables
# var_window_size_tune <- c(150L, 350L)
var_window_size_tune <- c(100L, 350L)
var_mp_threshold_tune <- c(0, 1)
var_time_constraint_tune <- c(750L, 2000L)
var_regime_threshold_tune <- c(0.2, 0.8)
var_regime_landmark_tune <- c(2, 6)
var_regime_landmark <- 3
# which tune algorithm?
# tune_grid, tune_bayes, tune_sim_anneal, tune_race_anova, tune_race_win_loss
var_grid_search <- "tune_bayes"
var_grid_size <- 10 # grid and race_* / can be a previous search result
var_tune_bayes_iter <- 200 # bayes
var_tune_bayes_initial <- 100 # bayes / can be a previous search result
var_tune_bayes_no_improve <- 30 # bayes
var_tune_sim_anneal_iter <- var_tune_bayes_iter # anneal
var_tune_sim_anneal_initial <- var_tune_bayes_initial # anneal / can be a previous search result
var_tune_sim_anneal_no_improve <- var_tune_bayes_no_improve # anneal
# splits
# initial split, 3/4 will hold 25% of the data for final, independent, performance.
var_initial_split_prop <- 3 / 4
var_vfolds <- 5 # for the inner resample
var_vfolds_repeats <- 2 # for the inner resample
# parallel
var_dopar_cores <- 20 # number of cores to use on tuning (inner resample)

var_verbose <- TRUE
var_save_workflow <- FALSE
var_save_pred <- TRUE


# # All configurations used different CPUs while running the code.
# plan(multisession) # create top-level processes
# plan(multicore) # create child processes
future::plan(future.callr::callr, workers = 10) # create child processes with a child process

tidymodels::tidymodels_prefer(quiet = TRUE)


# start debugme after loading all functions
# if (dev_mode) {
# debugme::debugme()
# }

# cat(.Random.seed)
# cat("\n\n\n\n")

list(
  tar_files_input(
    #### Pipeline: file_paths - Read files from directory ----
    file_paths,
    find_all_files(here::here("inst", "extdata", "afib_regimes"),
      data_type = "regimes",
      classes = var_classes_include
      # limit_per_class = 10
    )
  ),
  tar_target(
    #### Pipeline: dataset - Import Files to R and Select Datasets ----
    dataset,
    read_and_prepare_ecgs(file_paths,
      subset = var_subset,
      limit_per_class = var_limit_per_class,
      data_type = "regime",
      resample_from = var_resample_from,
      resample_to = var_resample_to,
      normalize = TRUE
    )
  ),
  tar_target(
    #### Pipeline: initial_resample - Tidy dataset and create the initial resample ----
    initial_resample,
    {
      tidy_dataset <- purrr::map_dfr(dataset, function(x) {
        regimes <- attr(x, "regimes")
        if (length(regimes) == 0) {
          return(NULL) # remove files that has no change in the subset
        }
        tibble::tibble(truth = list(regimes), ts = list(x$II))
      }, .id = "id")
      rsample::initial_split(tidy_dataset, prop = var_initial_split_prop)
    }
  ),
  tar_target(
    #### Pipeline: testing_split - Create the final testing split for the outer loop ----
    testing_split,
    {
      # outer loop, this will be evaluated last
      rsample::testing(initial_resample) # 5
    }
  ),
  tar_target(
    #### Pipeline: training_split - Create the training split for the inner loop ----
    training_split,
    {
      # outer-inner loop, this will be cross-validated
      rsample::training(initial_resample) # 15
    }
  ),
  tar_target(
    #### Pipeline: analysis_split - Subset the training split into analysis split (training) ----
    analysis_split,
    {
      # use the same seed for analysis and assessment to avoid the creation of
      # an intermediate redundant split
      my_seed <- tar_meta(training_split, seed)$seed
      set.seed(my_seed)
      validation_split <- rsample::vfold_cv(training_split, var_vfolds, var_vfolds_repeats)
      this_split <- NULL
      for (i in seq_along(validation_split$splits)) {
        this_split <- rsample::analysis(validation_split$splits[[i]]) %>%
          rsample::apparent() %>%
          dplyr::bind_rows(this_split)
      }

      result <- NULL
      for (i in seq_len(var_vfolds_repeats)) {
        mask <- seq.int(var_vfolds * (i - 1) + 1, var_vfolds * i)
        res <- rsample::manual_rset(this_split$splits[mask], id = glue_fmt("Fold{seq_len(var_vfolds):02d}")) %>%
          dplyr::mutate(
            rep = glue("Repeat{i}")
          )
        result <- dplyr::bind_rows(result, res)
      }

      # group by repeats, so targets will create multiple branches
      result <- result %>%
        dplyr::group_by(rep) %>%
        tar_group()
      result
    },
    iteration = "group"
  ),
  tar_target(
    #### Pipeline: assessment_split - Subset the training split into assessment split (test) ----
    assessment_split,
    {
      # use the same seed for analysis and assessment to avoid the creation of
      # an intermediate redundant split
      my_seed <- tar_meta(training_split, seed)$seed
      set.seed(my_seed)
      validation_split <- rsample::vfold_cv(training_split, var_vfolds, var_vfolds_repeats)
      this_split <- NULL
      for (i in seq_along(validation_split$splits)) {
        this_split <- rsample::assessment(validation_split$splits[[i]]) %>%
          rsample::apparent() %>%
          dplyr::bind_rows(this_split)
      }

      result <- NULL
      for (i in seq_len(var_vfolds_repeats)) {
        mask <- seq.int(var_vfolds * (i - 1) + 1, var_vfolds * i)
        res <- rsample::manual_rset(this_split$splits[mask], id = glue_fmt("Fold{seq_len(var_vfolds):02d}")) %>%
          dplyr::mutate(
            rep = glue("Repeat{i}")
          )
        result <- dplyr::bind_rows(result, res)
      }

      # group by repeats, so targets will create multiple branches
      result <- result %>%
        dplyr::group_by(rep) %>%
        tar_group()
      result
    },
    iteration = "group"
  ),
  tar_target(
    #### Pipeline: analysis_fitted - Here we will conduct the parameter optimizations ----
    analysis_fitted,
    {
      future::plan(future.callr::callr, workers = 10)
      source(here::here("scripts", "regimes", "parsnip_model.R"), encoding = "UTF-8")
      # A fix for targets branches that wipes off these classes
      class(analysis_split) <- c("manual_rset", "rset", class(analysis_split))

      floss_spec <-
        floss_regime_model(
          window_size = tune::tune(),
          time_constraint = tune::tune(),
          mp_threshold = tune::tune(),
          regime_threshold = tune::tune(),
          regime_landmark = var_regime_landmark
        ) %>%
        parsnip::set_engine("floss") %>%
        parsnip::set_mode("regression")

      floss_set <- tune::extract_parameter_set_dials(floss_spec)
      floss_set <- floss_set %>% stats::update(
        window_size = window_size_par(var_window_size_tune),
        mp_threshold = mp_threshold_par(var_mp_threshold_tune),
        time_constraint = time_constraint_par(var_time_constraint_tune),
        regime_threshold = regime_threshold_par(var_regime_threshold_tune)
        # regime_landmark = regime_landmark_par(var_regime_landmark_tune)
      )

      floss_rec <- recipes::recipe(x = head(analysis_split$splits[[1]]$data, 1)) %>%
        recipes::update_role(truth, new_role = "outcome") %>%
        recipes::update_role(id, new_role = "predictor") %>%
        recipes::update_role(ts, new_role = "predictor")

      # doMC::registerDoMC(cores = 8)
      if (var_dopar_cores > 1) {
        doParallel::registerDoParallel(cores = var_dopar_cores)
      }

      if (var_grid_search == "tune_grid") {
        floss_search_res <- floss_spec %>%
          tune::tune_grid(
            preprocessor = floss_rec,
            resamples = analysis_split,
            param_info = floss_set,
            grid = var_grid_size,
            metrics = yardstick::metric_set(floss_error_macro),
            control = tune::control_grid(
              verbose = var_verbose,
              allow_par = TRUE,
              save_workflow = var_save_workflow,
              save_pred = var_save_pred,
              parallel_over = "resamples"
            )
          )
      } else if (var_grid_search == "tune_bayes") {
        trade_off_decay <- function(iter) {
          expo_decay(iter, start_val = 0.01, limit_val = 0, slope = 0.25)
        }

        floss_search_res <- floss_spec %>%
          tune::tune_bayes(
            preprocessor = floss_rec,
            resamples = analysis_split,
            param_info = floss_set,
            initial = var_tune_bayes_initial,
            iter = var_tune_bayes_iter,
            metrics = yardstick::metric_set(floss_error_macro, floss_error_micro),
            objective = tune::exp_improve(trade_off_decay),
            control = tune::control_bayes(
              no_improve = var_tune_bayes_no_improve,
              verbose = var_verbose,
              save_workflow = var_save_workflow,
              save_pred = var_save_pred,
              parallel_over = "resamples"
            )
          )
      } else if (var_grid_search == "tune_race_win_loss") {
        floss_search_res <- floss_spec %>%
          finetune::tune_race_win_loss(
            preprocessor = floss_rec,
            resamples = analysis_split,
            param_info = floss_set,
            grid = var_grid_size,
            metrics = yardstick::metric_set(floss_error_macro, floss_error_micro),
            control = finetune::control_race(
              verbose_elim = TRUE,
              verbose = var_verbose,
              save_workflow = var_save_workflow,
              save_pred = var_save_pred,
              allow_par = TRUE,
              parallel_over = "resamples"
            )
          )
        # TODO: finetune::plot_race(floss_search_res)
      } else if (var_grid_search == "tune_race_anova") {
        floss_search_res <- floss_spec %>%
          finetune::tune_race_anova(
            preprocessor = floss_rec,
            resamples = analysis_split,
            param_info = floss_set,
            grid = var_grid_size,
            metrics = yardstick::metric_set(floss_error_macro, floss_error_micro),
            control = finetune::control_race(
              verbose_elim = TRUE,
              verbose = var_verbose,
              save_workflow = var_save_workflow,
              save_pred = var_save_pred,
              allow_par = TRUE,
              parallel_over = "resamples"
            )
          )
        # TODO: finetune::plot_race(floss_search_res)
      } else if (var_grid_search == "tune_sim_anneal") {
        floss_search_res <- floss_spec %>%
          finetune::tune_sim_anneal(
            preprocessor = floss_rec,
            resamples = analysis_split,
            iter = var_tune_sim_anneal_iter,
            initial = var_tune_sim_anneal_initial,
            param_info = floss_set,
            metrics = yardstick::metric_set(floss_error_macro, floss_error_micro),
            control = finetune::control_sim_anneal(
              verbose = var_verbose,
              save_workflow = var_save_workflow,
              save_pred = var_save_pred,
              no_improve = var_tune_sim_anneal_no_improve,
              parallel_over = "resamples"
            )
          )
      }

      floss_search_res <- clean_splits_data(floss_search_res)
      floss_search_res
    },
    pattern = map(analysis_split),
    iteration = "list" # thus the objects keep their attributes
  ),
  tar_target(
    #### Pipeline: analysis_evaluation - Here we select the best from each optimization split and test in a separate split ----
    analysis_evaluation,
    {
      future::plan(future.callr::callr, workers = 6)
      source(here::here("scripts", "regimes", "parsnip_model.R"), encoding = "UTF-8")

      # all_fits <- lst_to_df(analysis_fitted)
      all_fits <- analysis_fitted

      best_fits <- all_fits %>% tune::show_best("floss_error_macro", 5)
      folds <- assessment_split %>% dplyr::filter(rep == best_fits$rep[1])

      # must have var_vfolds^th folds
      tar_assert_true(nrow(folds) == var_vfolds)

      result <- NULL
      for (i in seq_len(nrow(best_fits))) {
        model <- best_fits[i, ]

        floss_spec <-
          floss_regime_model(
            window_size = model$window_size,
            time_constraint = model$time_constraint,
            mp_threshold = model$mp_threshold,
            regime_threshold = model$regime_threshold,
            regime_landmark = var_regime_landmark
          ) %>%
          parsnip::set_engine("floss") %>%
          parsnip::set_mode("regression")

        floss_rec <- recipes::recipe(x = head(folds$splits[[1]]$data, 1)) %>%
          recipes::update_role(truth, new_role = "outcome") %>%
          recipes::update_role(id, new_role = "predictor") %>%
          recipes::update_role(ts, new_role = "predictor")

        floss_wflow <-
          workflows::workflow() %>%
          workflows::add_model(floss_spec) %>%
          workflows::add_recipe(floss_rec)

        for (j in seq_len(nrow(folds))) {
          assessment_data <- folds$splits[[j]]$data

          model_fit <- floss_wflow %>%
            parsnip::fit(assessment_data)
          model_predicted <- model_fit %>%
            predict(assessment_data) %>%
            dplyr::bind_cols(assessment_data)
          eval <- floss_error_macro(model_predicted, truth = model_predicted$truth, estimate = model_predicted$.pred)
          eval <- model %>%
            dplyr::select(rep, window_size, time_constraint, mp_threshold, regime_threshold, .config) %>%
            dplyr::bind_cols(eval) %>%
            dplyr::mutate(id = glue::glue("Fold0{j}"))
          result <- dplyr::bind_rows(result, eval)
        }
      }

      result <- result %>% dplyr::arrange(.config, .estimate)
    },
    pattern = map(analysis_fitted),
    iteration = "list" # thus the objects keep their attributes
  ),
  tar_target(
    #### Pipeline: testing_evaluation - In the end, get the best from the inner resample and test with the testing split ----
    testing_evaluation,
    {
      future::plan(future.callr::callr, workers = 6)
      source(here::here("scripts", "regimes", "parsnip_model.R"), encoding = "UTF-8")
      best_parameters <- dplyr::bind_rows(analysis_evaluation) %>%
        dplyr::group_by(
          window_size, time_constraint,
          mp_threshold, regime_threshold, .metric, .estimator
        ) %>%
        dplyr::summarise(mean = mean(.estimate), std_err = sd(.estimate), n = dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(mean, std_err) %>%
        dplyr::slice_min(n = 5, order_by = mean)

      result <- NULL
      for (i in seq_len(nrow(best_parameters))) {
        floss_spec <-
          floss_regime_model(
            window_size = best_parameters$window_size[i],
            time_constraint = best_parameters$time_constraint[i],
            mp_threshold = best_parameters$mp_threshold[i],
            regime_threshold = best_parameters$regime_threshold[i],
            regime_landmark = var_regime_landmark
          ) %>%
          parsnip::set_engine("floss") %>%
          parsnip::set_mode("regression")

        floss_rec <- recipes::recipe(x = head(testing_split, 1)) %>%
          recipes::update_role(truth, new_role = "outcome") %>%
          recipes::update_role(id, new_role = "predictor") %>%
          recipes::update_role(ts, new_role = "predictor")

        floss_wflow <-
          workflows::workflow() %>%
          workflows::add_model(floss_spec) %>%
          workflows::add_recipe(floss_rec)

        model_fit <- floss_wflow %>%
          parsnip::fit(testing_split)
        model_predicted <- model_fit %>%
          predict(testing_split) %>%
          dplyr::bind_cols(testing_split)
        eval <- floss_error_macro(model_predicted, truth = model_predicted$truth, estimate = model_predicted$.pred)
        model_predicted <- model_predicted %>%
          dplyr::select(.sizes, .id, .pred, truth) %>%
          tidyr::nest(.predictions = everything())
        eval <- best_parameters[i, ] %>%
          dplyr::select(window_size, time_constraint, mp_threshold, regime_threshold) %>%
          dplyr::bind_cols(eval, model_predicted)

        result <- dplyr::bind_rows(result, eval)
      }
      result
    }
  )
)


# default: For space-filling or random grids, a marginal effect plot is created.
# autoplot(all_fits, type = "marginals") + labs(title = "Marginal plot", x = "Parameter value", y = "Performance") +
#   theme_bw()

# autoplot(all_fits, type = "parameters") + labs(title = "Parameter search", x = "Iterations", y = "Parameter value") +
#   theme_bw()

#  autoplot(all_fits, type = "performance", width = 0) + labs(title = "Performance", x = "Iterations", y = "Performance") +
#   theme_bw()

# best_model <- select_best(all_fits, metric = "floss_error")
# best_by_one_std_err <- select_by_one_std_err(all_fits, metric = "floss_error")
# best_by_pct_loss <- select_by_pct_loss(all_fits, metric = "floss_error", limit = 5)
# collect_metrics(all_fits, summarize = FALSE) %>%
#   group_by(id) %>%
#   summarise(mean = mean(.estimate), n = n(), std_err = sd(.estimate))

# best_param <- select_best(ames_iter_search, metric = "rmse")
# ames_iter_search %>%
#   filter_parameters(parameters = best_param) %>%
#   collect_metrics()
