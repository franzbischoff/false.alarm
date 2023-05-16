# Sys.setenv(TAR_PROJECT = "classifier")

# Errors: data_pos_neg_150_II
# data_pos_neg_150_PLETH, data_pos_neg_300_II, data_pos_neg_300_PLETH
# longer object length is not a multiple of shorter object length

# TODO: change pipeline to nested resamplng

# Load global config
source(here::here("scripts", "_globals.R"), local = .GlobalEnv, encoding = "UTF-8") # nolint
source(here::here("scripts", "classification", "pan_contrast.R"), local = .GlobalEnv, encoding = "UTF-8") # nolint
source(here::here("scripts", "helpers", "plot_contrast.R"), local = .GlobalEnv, encoding = "UTF-8") # nolint
source(here::here("scripts", "helpers", "pan_contrast_helpers.R"), local = .GlobalEnv, encoding = "UTF-8") # nolint


options(target_ds_path = here::here("inst", "extdata", "physionet")) # nolint
options(tidymodels.dark = TRUE) # nolint
options(progressr.enable = TRUE) # nolint

#### Pipeline: variable definitions ----
# signal sample frequency, this is a constant
const_sample_freq <- 250
const_signals <- c("time", "I", "II", "III", "ABP", "PLETH", "RESP")
# const_classes <- c("asystole", "bradycardia", "tachycardia", "fibv", "vtachy")

# var_resample_from <- 200
# var_resample_to <- const_sample_freq

# keep only the X filenames
# var_head <- 10
# The subset that will be keep from the dataset (seq.int(240 * const_sample_freq + 1, 300 * const_sample_freq) means the last 60 seconds)
# var_subset <- seq.int(240 * const_sample_freq + 1, 300 * const_sample_freq) # last 60 secs
var_subset <- seq.int(290 * const_sample_freq + 1, 300 * const_sample_freq) # last 10 secs
var_limit_per_class <- NULL

var_classes_include <- "vtachy"
var_classes_exclude <- NULL

var_signals_include <- "II"
var_signals_exclude <- setdiff(const_signals, var_signals_include)



#### Targets: Define targets options ----
# readRenviron(".Renviron")
# source("renv/activate.R")
# renv::install(c("dplyr", "rlang", "rsample", "tidyr"))
# use renv::install(".") to update the rcpp functions

tar_option_set(
  tidy_eval = TRUE,
  # library = "/workspace/.cache/R/renv/proj_libs/false.alarm-d6f1a0d1/R-4.3/x86_64-pc-linux-gnu",
  packages = c(
    "here", "glue", "false.alarm", "dplyr", "rlang", "rsample", "tidyr",
    "dials", "scales", "tibble", "parsnip", "yardstick", "purrr", "hardhat"
  ),
  format = "rds",
  memory = "transient",
  # debug = "find_shapelets",
  garbage_collection = TRUE
)

# var_shapelet_size <- c(120, 300, 1000) # c(30, 60, 120, 180, 300) # c(150, 300)
var_shapelet_sizes <- get_exp_dist_series(20, 400, 20) # c(20, 60, 100, 140, 180, 220, 260, 300)
var_positive <- TRUE # c(TRUE, FALSE)
var_num_shapelets <- 10
var_num_neighbors <- 10
var_min_corr_neighbors <- 0.85
var_pan_contrast <- seq(20, 1000, by = 50)

############
# tuning variables
# var_window_size_tune <- c(150L, 350L)
# var_window_size_tune <- c(25L, 26L)
# var_mp_threshold_tune <- c(0, 1)
# var_time_constraint_tune <- c(750L, 2000L)
# The subset that will be keep from the dataset (seq.int(290 * const_sample_freq + 1, 300 * const_sample_freq) means the last 10 seconds)
var_regime_threshold_tune <- c(0.05, 0.9)
var_regime_landmark_tune <- c(2, 10)
var_regime_landmark <- 3
# which tune algorithm?
# tune_grid, tune_bayes, tune_sim_anneal, tune_race_anova, tune_race_win_loss
var_grid_search <- "tune_grid"
var_grid_size <- 1000 # grid and race_* / can be a previous search result
var_tune_bayes_iter <- 5 # bayes
var_tune_bayes_initial <- 200 # bayes / can be a previous search result
var_tune_bayes_no_improve <- 5 # bayes
var_tune_sim_anneal_iter <- var_tune_bayes_iter # anneal
var_tune_sim_anneal_initial <- var_tune_bayes_initial # anneal / can be a previous search result
var_tune_sim_anneal_no_improve <- var_tune_bayes_no_improve # anneal
# splits
# initial split, 3/4 will hold 25% of the data for final, independent, performance.
var_initial_split_prop <- 3 / 4
var_vfolds <- 5 # for the inner resample
var_vfolds_repeats <- 2 # for the inner resample
# parallel
var_dopar_cores <- 5 # number of cores to use on tuning (inner resample)
var_future_workers <- 3

var_verbose <- TRUE
var_save_workflow <- FALSE
var_save_pred <- TRUE


# # All configurations used different CPUs while running the code.
# plan(multisession) # create top-level processes
# plan(multicore) # create child processes
future::plan(future.callr::callr, workers = var_future_workers) # create child processes with a child process

tidymodels::tidymodels_prefer(quiet = TRUE)


# start debugme after loading all functions
# if (dev_mode) {
debugme::debugme()
# }

# cat(.Random.seed)
# cat("\n\n\n\n")


#### Pipeline: Start ----
list(
  tar_files_input(
    #### Pipeline: file_paths - Read files from directory ----
    file_paths,
    find_all_files(here::here("inst", "extdata", "physionet"),
      data_type = "alarm",
      classes = var_classes_include
      # limit_per_class = 10
    )
  ),
  tar_target(
    #### Pipeline: Import the last 10 seconds of all TRUE and FALSE alarms ----
    dataset,
    {
      temp <- read_and_prepare_ecgs(file_paths,
        subset = var_subset,
        limit_per_class = var_limit_per_class,
        normalize = TRUE
      )
      reshape_ds_by_truefalse(temp, var_signals_include, all_signals = FALSE)
      # headers: file: filename; class: record class (fib, vfib, etc); values: the recording; alarm: true positive or false positive;
      # class_alarm: classcolumn_alarmcolumn // class_alarm is a dummy variable, because the `rsample` package does
      # not accept more than one variable for stratification.
    }
  ),
  tar_target(
    #### Pipeline: initial_resample - Tidy dataset and create the initial resample ----
    initial_resample,
    {
      rsample::initial_split(dataset[[1]], prop = var_initial_split_prop)
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
  ###### Inner Resample ######
  tar_target(
    #### Pipeline: assessment_split - Subset the training split into assessment split (test) ----
    contrast_profiles,
    {
      # TODO: change to analysis_split
      shapelet_sizes <- var_shapelet_sizes

      class(assessment_split) <- c("manual_rset", "rset", class(assessment_split))

      res <- list()
      for (i in seq_len(var_vfolds)) {
        fold <- rsample::get_rsplit(assessment_split, i)
        res[[i]] <- contrastprofile_topk(fold, shapelet_sizes, var_num_shapelets, n_jobs = var_future_workers, TRUE)
      }

      res
    },
    pattern = map(assessment_split),
    iteration = "list"
  ),
  tar_target(
    find_shapelets,
    {
      res <- list()
      for (i in seq_len(var_vfolds)) {
        cli::cli_alert_info("Scores by segment, fold {i}.")
        score <- score_by_segment_window(contrast_profiles[[i]]$positive, contrast_profiles[[i]]$negative, contrast_profiles[[i]]$pan)
        cli::cli_alert_info("Finding solutions, fold {i}.")
        solutions <- find_solutions(score, cov = 10, n = 10, rep = 100000, red = 10, n_jobs = 2)

        if (length(solutions) == 0) {
          res[[i]] <- NULL
        } else {
          cli::cli_alert_info("Filtering best solutions, fold {i}.")
          res[[i]] <- filter_best_solutions(solutions, 2)
        }
      }
      res
    },
    pattern = map(contrast_profiles),
    iteration = "list"
  ),
  tar_target(
    plot_profiles,
    {
      branch_name <- tar_name()
      max_size <- max(var_shapelet_sizes)
      plots <- list()
      for (i in seq_len(var_vfolds)) {
        plots[[i]] <- plot_best_candidates(find_shapelets, contrast_profiles, fold = i, max_size = max_size)
      }
      s <- svglite::svgstring(10, 15,
        web_fonts = list("https://fonts.googleapis.com/css?family=Roboto:400,400i,700,700i")
        # fix_text_size = FALSE,
        # standalone = FALSE
      )

      plt <- patchwork::wrap_plots(plots) +
        patchwork::plot_annotation(
          title = branch_name,
          theme = ggplot2::theme(plot.title = ggplot2::element_text(family = "Roboto"))
        )
      print(plt)
      dev.off()
      readr::write_file(s(), file = here::here("output", glue::glue("Shapes_{branch_name}.svg")))

      plt
    },
    pattern = map(find_shapelets, contrast_profiles),
    iteration = "list"
  )
  # tar_target(
  #   best_shapelets,
  #   {
  #     # algorithm for selecting the best shapelet
  #   },
  #   pattern = map(contrast_profiles),
  #   iteration = "list"
  # ),
  # tar_target(
  #   train_classifier,
  #   {
  #     # train a classifier based on the best shapelets
  #   },
  #   pattern = map(best_shapelets),
  #   iteration = "list"
  # ),
  # tar_target(
  #   test_classifier,
  #   {
  #     # test the classifier on the assessment split
  #   },
  #   pattern = map(assessment_split),
  #   iteration = "list"
  # ),
  # ### Evaluate on test set
  # ####
  # inner_resample <- tar_map(
  #   list(window_size_map = c(25, 50, 75, 100, 125, 150, 175, 200)),
  #   tar_target(
  #     #### Pipeline: analysis_fitted - Here we will conduct the parameter optimizations ----
  #     analysis_fitted,
  #     {
  #       future::plan(future.callr::callr, workers = var_future_workers)
  #       # source(here::here("scripts", "regimes", "parsnip_model.R"), encoding = "UTF-8")
  #       # A fix for targets branches that wipes off these classes
  #       # analysis_split <- analysis_split[1, ] # this is for fast testing, uses only the first split
  #       class(analysis_split) <- c("manual_rset", "rset", class(analysis_split))

  #       floss_spec <-
  #         floss_regime_model(
  #           window_size = tune::tune(),
  #           time_constraint = 0L,
  #           mp_threshold = 0.0,
  #           regime_threshold = tune::tune(),
  #           regime_landmark = tune::tune()
  #         ) %>%
  #         parsnip::set_engine("floss") %>%
  #         parsnip::set_mode("regression")

  #       floss_set <- tune::extract_parameter_set_dials(floss_spec)
  #       floss_set <- floss_set %>% stats::update(
  #         window_size = window_size_par(c(window_size_map, window_size_map + 1)),
  #         # mp_threshold = mp_threshold_par(var_mp_threshold_tune),
  #         # time_constraint = time_constraint_par(var_time_constraint_tune),
  #         regime_threshold = regime_threshold_par(var_regime_threshold_tune, trans_round(0.05)),
  #         regime_landmark = regime_landmark_par(var_regime_landmark_tune)
  #       )

  #       floss_rec <- recipes::recipe(x = head(analysis_split$splits[[1]]$data, 1)) %>%
  #         recipes::update_role(truth, new_role = "outcome") %>%
  #         recipes::update_role(id, new_role = "predictor") %>%
  #         recipes::update_role(ts, new_role = "predictor")

  #       # doMC::registerDoMC(cores = 8)
  #       if (var_dopar_cores > 1) {
  #         doParallel::registerDoParallel(cores = var_dopar_cores)
  #       }

  #       # floss_wflow <-
  #       #   workflows::workflow() %>%
  #       #   workflows::add_model(floss_spec) %>%
  #       #   workflows::add_recipe(floss_rec)

  #       # fitted_wflow <- floss_wflow %>% parsnip::fit(analysis_split$splits[[1]]$data)

  #       if (var_grid_search == "tune_grid") {
  #         floss_search_res <- floss_spec %>%
  #           tune::tune_grid(
  #             preprocessor = floss_rec,
  #             resamples = analysis_split,
  #             param_info = floss_set,
  #             grid = var_grid_size,
  #             metrics = yardstick::metric_set(floss_error_macro),
  #             control = tune::control_grid(
  #               verbose = var_verbose,
  #               allow_par = TRUE,
  #               save_workflow = var_save_workflow,
  #               save_pred = var_save_pred,
  #               parallel_over = "resamples"
  #             )
  #           )
  #       } else if (var_grid_search == "tune_bayes") {
  #         trade_off_decay <- function(iter) {
  #           tune::expo_decay(iter, start_val = 0.01, limit_val = 0, slope = 0.25)
  #         }

  #         floss_search_res <- floss_spec %>%
  #           tune::tune_bayes(
  #             preprocessor = floss_rec,
  #             resamples = analysis_split,
  #             param_info = floss_set,
  #             initial = var_tune_bayes_initial,
  #             iter = var_tune_bayes_iter,
  #             metrics = yardstick::metric_set(floss_error_macro, floss_error_micro),
  #             objective = tune::exp_improve(trade_off_decay),
  #             control = tune::control_bayes(
  #               no_improve = var_tune_bayes_no_improve,
  #               verbose = var_verbose,
  #               save_workflow = var_save_workflow,
  #               save_pred = var_save_pred,
  #               parallel_over = "resamples"
  #             )
  #           )
  #       } else if (var_grid_search == "tune_race_win_loss") {
  #         floss_search_res <- floss_spec %>%
  #           finetune::tune_race_win_loss(
  #             preprocessor = floss_rec,
  #             resamples = analysis_split,
  #             param_info = floss_set,
  #             grid = var_grid_size,
  #             metrics = yardstick::metric_set(floss_error_macro, floss_error_micro),
  #             control = finetune::control_race(
  #               verbose_elim = TRUE,
  #               verbose = var_verbose,
  #               save_workflow = var_save_workflow,
  #               save_pred = var_save_pred,
  #               allow_par = TRUE,
  #               parallel_over = "resamples"
  #             )
  #           )
  #         # TODO: finetune::plot_race(floss_search_res)
  #       } else if (var_grid_search == "tune_race_anova") {
  #         floss_search_res <- floss_spec %>%
  #           finetune::tune_race_anova(
  #             preprocessor = floss_rec,
  #             resamples = analysis_split,
  #             param_info = floss_set,
  #             grid = var_grid_size,
  #             metrics = yardstick::metric_set(floss_error_macro, floss_error_micro),
  #             control = finetune::control_race(
  #               verbose_elim = TRUE,
  #               verbose = var_verbose,
  #               save_workflow = var_save_workflow,
  #               save_pred = var_save_pred,
  #               allow_par = TRUE,
  #               parallel_over = "resamples"
  #             )
  #           )
  #         # TODO: finetune::plot_race(floss_search_res)
  #       } else if (var_grid_search == "tune_sim_anneal") {
  #         floss_search_res <- floss_spec %>%
  #           finetune::tune_sim_anneal(
  #             preprocessor = floss_rec,
  #             resamples = analysis_split,
  #             iter = var_tune_sim_anneal_iter,
  #             initial = var_tune_sim_anneal_initial,
  #             param_info = floss_set,
  #             metrics = yardstick::metric_set(floss_error_macro, floss_error_micro),
  #             control = finetune::control_sim_anneal(
  #               verbose = var_verbose,
  #               save_workflow = var_save_workflow,
  #               save_pred = var_save_pred,
  #               no_improve = var_tune_sim_anneal_no_improve,
  #               parallel_over = "resamples"
  #             )
  #           )
  #       }

  #       floss_search_res <- clean_splits_data(floss_search_res)
  #       floss_search_res
  #     },
  #     pattern = map(analysis_split),
  #     iteration = "list" # thus the objects keep their attributes
  #   ),
  #   tar_target(
  #     #### Pipeline: analysis_evaluation - Here we select the best from each optimization split and test in a separate split ----
  #     analysis_evaluation,
  #     {
  #       # source(here::here("scripts", "regimes", "parsnip_model.R"), encoding = "UTF-8")

  #       # all_fits will contain a tibble from a mapped window size * repeat, i.e., analysis_fitted_25 repeat1
  #       all_fits <- analysis_fitted


  #       # best_models are the best models from some window_size, and current repeat. sorted by the mean over all folds
  #       best_models <- all_fits %>% tune::show_best("floss_error_macro", 5)

  #       if (var_dopar_cores > 1) {
  #         doParallel::registerDoParallel(cores = var_dopar_cores)
  #       }


  #       result <- NULL
  #       # here we will iterate over each fold. The data is different, so we fit a model for each fold.
  #       for (i in seq_len(var_vfolds)) {
  #         fold <- i
  #         repet <- best_models$rep[1]

  #         # get only the data from the current fold and repeat
  #         resample <- assessment_split %>% dplyr::filter(id == glue::glue("Fold0{fold}"), rep == repet)
  #         class(resample) <- class(assessment_split) # fix for fit_resamples

  #         # so we end up with a tibble with all TS from this fold (id, truth and ts)
  #         assessment_data <- resample$splits[[1]]$data %>% dplyr::arrange(id)

  #         # make a generic recipe, it will be all the same anyway
  #         floss_rec <- recipes::recipe(x = head(assessment_data, 1)) %>%
  #           recipes::update_role(truth, new_role = "outcome") %>%
  #           recipes::update_role(id, new_role = "predictor") %>%
  #           recipes::update_role(ts, new_role = "predictor")

  #         # now, fit the model with the current window_size, don't care the other parameters
  #         model <- best_models[1, ]

  #         floss_spec <-
  #           floss_regime_model(
  #             window_size = model$window_size,
  #             time_constraint = 0,
  #             mp_threshold = 0,
  #             regime_threshold = model$regime_threshold,
  #             regime_landmark = model$regime_landmark
  #           ) %>%
  #           parsnip::set_engine("floss") %>%
  #           parsnip::set_mode("regression")

  #         floss_wflow <-
  #           workflows::workflow() %>%
  #           workflows::add_model(floss_spec) %>%
  #           workflows::add_recipe(floss_rec)

  #         model_fit <- floss_wflow %>%
  #           parsnip::fit(assessment_data)


  #         # now we have the model_fit with the current window_size on all ts of this fold, remember the id and truth must match when evaluate

  #         # now the submodels:
  #         # multi_pred will contain all the predictions from all best models in all ts from this fold
  #         multi_pred <- workflows::extract_fit_parsnip(model_fit) %>%
  #           multi_predict(
  #             new_data = assessment_data,
  #             regime_threshold = best_models$regime_threshold,
  #             regime_landmark = best_models$regime_landmark
  #           )

  #         all_preds <- multi_pred %>%
  #           tidyr::unnest(.pred) %>%
  #           dplyr::arrange(.id)

  #         for (j in seq_len(nrow(best_models))) {
  #           model <- best_models[j, ]
  #           estimates <- all_preds %>% dplyr::filter(
  #             regime_threshold == model$regime_threshold,
  #             regime_landmark == model$regime_landmark
  #           )
  #           eval <- floss_error_vec(truth = assessment_data$truth, estimate = estimates$.pred, data_size = estimates$.sizes, estimator = "macro")
  #           eval <- tibble::tibble(.metric = "floss_error_macro", .estimator = "macro", .estimate = eval)
  #           eval <- model %>%
  #             dplyr::select(rep, window_size, regime_threshold, regime_landmark, .config) %>%
  #             dplyr::mutate(fold = glue::glue("Fold0{fold}")) %>%
  #             dplyr::bind_cols(eval)
  #           result <- dplyr::bind_rows(result, eval)
  #         }
  #       }
  #       result <- result %>% dplyr::arrange(.estimate)
  #     },
  #     pattern = map(analysis_fitted),
  #     iteration = "list" # thus the objects keep their attributes
  #   )
  # ),
  # tar_combine(
  #   name = combined,
  #   inner_resample$analysis_evaluation,
  #   use_names = FALSE,
  #   command = bind_rows(vctrs::vec_c(!!!.x))
  # ),
  # tar_target(
  #   #### Pipeline: testing_evaluation - In the end, get the best from the inner resample and test with the testing split ----
  #   testing_evaluation,
  #   {
  #     # source(here::here("scripts", "regimes", "parsnip_model.R"), encoding = "UTF-8")
  #     # best_parameters <- purrr::map_dfr(combined, ~ .x %>% dplyr::top_n(n = 3, wt = .estimate))
  #     best_parameters <- combined %>%
  #       dplyr::group_by(window_size, regime_threshold, regime_landmark) %>%
  #       dplyr::summarize(mean = mean(.estimate), sd = sd(.estimate)) %>%
  #       dplyr::ungroup() %>%
  #       dplyr::arrange(mean, sd) %>%
  #       dplyr::slice_head(n = 6)


  #     if (var_dopar_cores > 1) {
  #       doParallel::registerDoParallel(cores = var_dopar_cores)
  #     }

  #     result <- NULL
  #     for (i in seq_len(nrow(best_parameters))) {
  #       floss_spec <-
  #         floss_regime_model(
  #           window_size = best_parameters$window_size[i],
  #           time_constraint = 0,
  #           mp_threshold = 0,
  #           regime_threshold = best_parameters$regime_threshold[i],
  #           regime_landmark = best_parameters$regime_landmark[i]
  #         ) %>%
  #         parsnip::set_engine("floss") %>%
  #         parsnip::set_mode("regression")

  #       floss_rec <- recipes::recipe(x = head(testing_split, 1)) %>%
  #         recipes::update_role(truth, new_role = "outcome") %>%
  #         recipes::update_role(id, new_role = "predictor") %>%
  #         recipes::update_role(ts, new_role = "predictor")

  #       floss_wflow <-
  #         workflows::workflow() %>%
  #         workflows::add_model(floss_spec) %>%
  #         workflows::add_recipe(floss_rec)

  #       model_fit <- floss_wflow %>%
  #         parsnip::fit(testing_split)
  #       model_predicted <- model_fit %>%
  #         predict(testing_split) %>%
  #         dplyr::bind_cols(testing_split)
  #       eval <- floss_error(model_predicted, truth = model_predicted$truth, estimate = model_predicted$.pred, estimator = "macro")
  #       eval <- best_parameters[i, ] %>%
  #         dplyr::select(window_size, regime_threshold, regime_landmark) %>%
  #         dplyr::bind_cols(eval)

  #       result <- result %>% dplyr::bind_rows(eval)
  #     }
  #     result
  #   }
  # )

  # tar_map(
  #   values = list(map_signals_include = var_signals_include),
  #   tar_map(
  #     values = list(map_positive = var_positive),
  #     tar_target(
  #       #### Pipeline: Build the positive and negative streams using all classes, with signal validation
  #       data_pos_neg_pan,
  #       build_pos_neg(ds_initial_split,
  #         signal = map_signals_include,
  #         shapelet_size = var_pan_contrast,
  #         positive = map_positive,
  #         validate = TRUE,
  #         same_class = TRUE
  #       )
  #     ),
  #     tar_target(
  #       pancontrast,
  #       pan_contrast(data_pos_neg_pan, # tar_make(pancontrast_TRUE_II)
  #         signal = map_signals_include,
  #         shapelet_sizes = var_pan_contrast
  #       )
  #     ),
  #     tar_target(
  #       #### Pipeline: Build the positive and negative streams using all classes, with signal validation
  #       data_all_pos_neg_pan,
  #       build_pos_neg(ds_initial_split,
  #         signal = map_signals_include,
  #         shapelet_size = var_pan_contrast,
  #         positive = map_positive,
  #         validate = TRUE,
  #         same_class = FALSE
  #       )
  #     ),
  #     tar_target(
  #       pan_allcontrast, # tar_make(pan_allcontrast_TRUE_II)
  #       pan_contrast(data_all_pos_neg_pan,
  #         signal = map_signals_include,
  #         shapelet_sizes = var_pan_contrast
  #       )
  #     ),
  #     tar_map(
  #       values = list(map_shapelet_size = var_shapelet_size),
  #       # First draft, not following parsnip rules: https://tidymodels.github.io/model-implementation-principles/function-interfaces.html
  #       tar_target(
  #         #### Pipeline: Build the positive and negative streams, with signal validation
  #         data_pos_neg,
  #         build_pos_neg(ds_initial_split,
  #           signal = map_signals_include,
  #           shapelet_size = map_shapelet_size,
  #           positive = map_positive,
  #           validate = TRUE,
  #           same_class = TRUE
  #         )
  #       ),
  #       tar_target(
  #         #### Pipeline: Build the positive and negative streams using all classes, with signal validation
  #         data_all_pos_neg,
  #         build_pos_neg(ds_initial_split,
  #           signal = map_signals_include,
  #           shapelet_size = map_shapelet_size,
  #           positive = map_positive,
  #           validate = TRUE,
  #           same_class = FALSE
  #         )
  #       ),
  #       tar_target(
  #         #### Pipeline: Computes the AA - AB difference.
  #         data_shapelets,
  #         find_k_shapelets(data_pos_neg,
  #           signal = map_signals_include,
  #           shapelet_size = map_shapelet_size,
  #           num_shapelets = var_num_shapelets
  #         )
  #       ),
  #       tar_target(
  #         #### Pipeline: Computes the AA - AB difference.
  #         data_all_shapelets,
  #         find_k_shapelets(data_all_pos_neg,
  #           signal = map_signals_include,
  #           shapelet_size = map_shapelet_size,
  #           num_shapelets = var_num_shapelets
  #         )
  #       ),
  #       tar_target(
  #         #### Pipeline: Computes the AA - AB difference.
  #         data_neighbors,
  #         find_k_neighbors(data_pos_neg,
  #           data_shapelets,
  #           signal = map_signals_include,
  #           n_neighbors = var_num_neighbors,
  #           corr_min = var_min_corr_neighbors,
  #           exclusion_zone = 0.5
  #         )
  #       ),
  #       tar_target(
  #         #### Pipeline: Computes the AA - AB difference.
  #         data_all_neighbors,
  #         find_k_neighbors(data_all_pos_neg,
  #           data_all_shapelets,
  #           signal = map_signals_include,
  #           n_neighbors = var_num_neighbors,
  #           corr_min = var_min_corr_neighbors,
  #           exclusion_zone = 0.5
  #         )
  #       )
  #     )
  #   )
  # )
)

# error on data_neighbors
# <simpleError in mass3_rcpp(query_window, data, as.integer(pre_obj$data_size),
#     as.integer(pre_obj$window_size), pre_obj$data_mean, pre_obj$data_sd,
#     pre_obj$query_mean[index], pre_obj$query_sd[index], k = 4096): c++ exception (unknown reason)>

# b_fit_models2 <- tar_target(
#   try_classify,
#   {
#     analysis_set <- rsample::analysis(initial_split[["II"]])
#     w <- seq(60, 500, by = 10) # size of the shapelets, can be tuned for each class

#     # which classes are present in the dataset?
#     classes <- unique(analysis_set$class)

#     for (cl in classes) {
#       cat("Starting Class ", cl, "\n")
#       data_class <- analysis_set %>% dplyr::filter(class == cl)

#       for (i in seq_len(nrow(data_class))) {
#         bsf_min <- Inf
#         bsf_class <- NULL

#         for (snip in classes) {
#           query <- class_shapelets[[snip]]$plato
#           pre <- false.alarm::mass_pre(data_class$values[[i]], w, query)
#           dp <- false.alarm::mass(pre, data_class$values[[i]], query)
#           curr_min <- min(dp$distance_profile)

#           if (bsf_min > curr_min) {
#             bsf_min <- curr_min
#             bsf_class <- snip
#           }
#         }

#         cat("Class: ", cl, " Answer: ", bsf_class, "min: ", bsf_min, "\n")

#         # # profile <- mpx(data_class$values[[i]], w, joint_shapelets)
#         # profile <- mpx(joint_shapelets, w, data_class$values[[i]])
#         # cat("MP: ", length(profile$matrix_profile), " Snip: ", length(joint_shapelets), " Data: ", length(data_class$values[[i]]), "\n")
#         # # cat("class ", cl, ": ", floor(profile$profile_index[which.min(profile$matrix_profile)] / w) + 1, "\n")
#         # cat("class ", cl, ": ", floor(which.min(profile$matrix_profile) / w) + 1, "\n")
#       }
#     }
#   }
# )

# tar_target(
#   #### Pipeline: Fit the best model and test on the test set ----
#   fit_best_model,
#   {
#     # Get the best model based on some metric: https://yardstick.tidymodels.org/articles/metric-types.html
#     best_model <- tune::select_best(fitted_models, "roc_auc")
#     # hack to get the model_spec and formula from the last target
#     wf <- attr(fitted_models, "workflow")
#     model <- tune::extract_spec_parsnip(wf)
#     formula <- tune::extract_preprocessor(wf)

#     # update the model_spec with the best parameters
#     model <- model %>% tune::finalize_model(best_model)

#     # last_fit() emulates the process where, after determining the best model, the final fit
#     # on the entire training set is needed and is then evaluated on the test set.
#     final_fit <- model %>% tune::last_fit(initial_split[[serie]], preprocessor = formula)

#     # metrics <- tune::collect_metrics(final_fit)
#     final_fit
#   }
# )
# )

# b_fit_models_old <- tar_map(
#   values = list(serie = var_signals_include),
#   tar_target(
#     #### Pipeline: Fit models over a cross validation framework using the training set ----
#     # more methods: https://rsample.tidymodels.org/reference/index.html#section-resampling-methods
#     fitted_models,
#     fit_models(initial_split[[serie]],
#       strata = "alarm",
#       arguments = list(
#         resampling = rsample::vfold_cv,
#         folds = 5,
#         repeats = 3,
#         model = parsnip::mlp,
#         tune = 10,
#         formula = as.formula(alarm ~ val),
#         metric = yardstick::metric_set(yardstick::roc_auc) # https://yardstick.tidymodels.org/articles/metric-types.html
#       )
#     )
#   ),
#   tar_target(
#     #### Pipeline: Fit the best model and test on the test set ----
#     fit_best_model,
#     {
#       # Get the best model based on some metric: https://yardstick.tidymodels.org/articles/metric-types.html
#       best_model <- tune::select_best(fitted_models, "roc_auc")
#       # hack to get the model_spec and formula from the last target
#       wf <- attr(fitted_models, "workflow")
#       model <- tune::extract_spec_parsnip(wf)
#       formula <- tune::extract_preprocessor(wf)

#       # update the model_spec with the best parameters
#       model <- model %>% tune::finalize_model(best_model)

#       # last_fit() emulates the process where, after determining the best model, the final fit
#       # on the entire training set is needed and is then evaluated on the test set.
#       final_fit <- model %>% tune::last_fit(initial_split[[serie]], preprocessor = formula)

#       # metrics <- tune::collect_metrics(final_fit)
#       final_fit
#     }
#   )
# )

# TODO: tuning, augmenting, out-of-sample, https://www.tidymodels.org/learn/work/tune-svm/


# R includes k-means, and the "flexclust" package can do k-means++
# tar_load(evaluate_models_II)
# evaluate_models_II %>%
#   tune::collect_predictions() %>%
#   yardstick::roc_curve(alarm, .pred_true) %>%
#   autoplot()
# tar_option_set(debug="evaluate_models_II")
# tar_make(names = evaluate_models_II, callr_function = NULL)

# rolling_origin {rsample}

#### Pipeline: Join targets ----
# list(r_input, r_dataset, b_initial_split, b_data_pos_neg, b_find_shapelets,
#   b_find_neg_shapelets, b_data_pos_neg, b_find_val_shapelets, b_find_neg_val_shapelets)

#### Pipeline: End ----
