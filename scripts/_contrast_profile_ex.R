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
var_pan_contrast <- seq(20, 470, by = 50)

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
var_vfolds <- 2 # for the inner resample
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
      res <- reshape_ds_by_truefalse(temp, var_signals_include, all_signals = FALSE)

      aa <- round(runif(50, 1, 331))
      res$II <- res$II[aa, ]
      res
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
        this_split <- rsample::analysis(validation_split$splits[[i]]) |>
          rsample::apparent() |>
          dplyr::bind_rows(this_split)
      }

      result <- NULL
      for (i in seq_len(var_vfolds_repeats)) {
        mask <- seq.int(var_vfolds * (i - 1) + 1, var_vfolds * i)
        res <- rsample::manual_rset(this_split$splits[mask], id = glue_fmt("Fold{seq_len(var_vfolds):02d}")) |>
          dplyr::mutate(
            rep = glue("Repeat{i}")
          )
        result <- dplyr::bind_rows(result, res)
      }

      # group by repeats, so targets will create multiple branches
      result <- result |>
        dplyr::group_by(rep) |>
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
        this_split <- rsample::assessment(validation_split$splits[[i]]) |>
          rsample::apparent() |>
          dplyr::bind_rows(this_split)
      }

      result <- NULL
      for (i in seq_len(var_vfolds_repeats)) {
        mask <- seq.int(var_vfolds * (i - 1) + 1, var_vfolds * i)
        res <- rsample::manual_rset(this_split$splits[mask], id = glue_fmt("Fold{seq_len(var_vfolds):02d}")) |>
          dplyr::mutate(
            rep = glue("Repeat{i}")
          )
        result <- dplyr::bind_rows(result, res)
      }

      # group by repeats, so targets will create multiple branches
      result <- result |>
        dplyr::group_by(rep) |>
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
      shapelet_sizes <- var_shapelet_sizes

      class(analysis_split) <- c("manual_rset", "rset", class(analysis_split))

      res <- list()
      for (i in seq_len(var_vfolds)) {
        fold <- rsample::get_rsplit(analysis_split, i)
        res[[i]] <- contrastprofile_topk(fold, shapelet_sizes, var_num_shapelets, n_jobs = var_future_workers, TRUE)
      }

      res
    },
    pattern = map(analysis_split),
    iteration = "list"
  ),

  # strOptions(strict.width = "no", digits.d = 3, vec.len = 0.5,
  #            list.len = 3, deparse.lines = NULL,
  #            drop.deparse.attr = TRUE)



  # tar_target(
  #   #### Pipeline: analysis_fitted - Here we will conduct the parameter optimizations ----
  #   analysis_fitted,
  #   {
  #     # source(here::here("scripts", "classification", "parsnip_model.R"), encoding = "UTF-8")

  #     contrast_spec <-
  #       contrast_model(
  #         # coverage_quantiles = tune::tune(), # score_by_segment_window
  #         num_shapelets = tune::tune(), # find_solutions
  #         redundancy = tune::tune() # find_solutions
  #       ) |>
  #       parsnip::set_engine("contrast_profile") |>
  #       parsnip::set_mode("classification")

  #     # filter_best_solutions
  #   },
  #   pattern = map(contrast_profiles),
  #   iteration = "list" # thus the objects keep their attributes
  # ),
  tar_target(
    #### Pipeline: score_by_segment - Preparation of the data: the model's data is the shapelets with metadata ----
    score_by_segment,
    {
      res <- list()
      for (i in seq_len(var_vfolds)) {
        cli::cli_alert_info("Scores by segment, fold {i}.")
        # These parameter can be tuned on `recipes`. These default values seems to be good enough
        tune1 <- 0.1
        tune2 <- 1 / 3
        score <- score_by_segment_window(contrast_profiles[[i]]$positive,
          contrast_profiles[[i]]$negative, contrast_profiles[[i]]$pan,
          quantiles = c(tune1, tune2)
        )
        res[[i]] <- score
      }
      res
    },
    pattern = map(contrast_profiles),
    iteration = "list"
  ),
  tar_target(
    #### Pipeline: find_shapelets - This is the model fit. ----
    find_shapelets,
    {
      # Here we can try: fitting all possible solutions and later score them and finally try
      # to find which metadata is the best to filter the solutions
      # Or, we can try to use some heuristics to find the best metadata for the solutions
      # These parameters are tuned on `parsnip`/`tune`
      # Currently the parameter `n` draws randomically 1 to `n` samples from the pan contrast profile
      # We can try to use a fixed number of samples during the parameter optimization
      res <- list()
      for (i in seq_len(var_vfolds)) {
        cli::cli_alert_info("Finding solutions, fold {i}.")
        tune3 <- 10 # this could be tuned, but some trials shows that limiting to smaller K's doesn't increase the performance
        solutions <- find_solutions(score_by_segment[[i]],
          min_cov = 10,
          max_shapelets = 20, # this can be more than topk
          rep = 5000,
          max_red = 10,
          max_k = tune3,
          n_jobs = 6
        )

        if (length(solutions) == 0) {
          res[i] <- list(NULL)
        } else {
          res[[i]] <- solutions
        }
      }
      res
    },
    pattern = map(score_by_segment),
    iteration = "list"
  ),
  tar_target(
    #### Pipeline: test_classifiers_self - This is the current score function. ----
    test_classifiers_self,
    {
      # With the results of this step, plus the fitted solutions, we need to find which
      # metadata is the best to filter the solutions
      class(analysis_split) <- c("manual_rset", "rset", class(analysis_split))

      res <- list()
      for (i in seq_len(var_vfolds)) {
        fold <- rsample::get_rsplit(analysis_split, i)
        res[[i]] <- list()
        shapelets <- find_shapelets[[i]]

        # the `compute_metrics_topk` function may need testing on the `TRUE` criteria
        # currently, if `ANY` shapelet matches, it is considered a positive
        # as alternative we can try to use `ALL`, `HALF` or other criteria
        res[[i]] <- compute_metrics_topk(fold, shapelets, 6, TRUE)
      }

      res # list(fold = res, overall = overall)


      # aa <- tibble::as_tibble(purrr::transpose(test_classifiers_self[[1]][[i]]))
      # aa <- dplyr::mutate_all(aa, as.numeric)
      # aa <- dplyr::bind_cols(find_shapelets[[1]][[i]], aa) |>
      #   dplyr::select(-data) |>
      #   dplyr::mutate(coverage = as.numeric(coverage), redundancy = as.numeric(redundancy))
      # bb <- dplyr::bind_rows(bb, aa)
      # i <- i + 1
      # summary(bb)
      # library(corrgram)
      # corrgram(bb,
      #   order = FALSE,
      #   lower.panel = panel.ellipse, upper.panel = panel.cor,
      #   text.panel = panel.txt
      # )
      # GGally::ggpairs(bb, aes(alpha = 0.05), lower = list(continuous = "smooth"))
    },
    pattern = map(find_shapelets, analysis_split),
    iteration = "list"
  ),
  tar_target(
    best_shapelets,
    {
      # Here we test the solutions we chose on the assessment split

      res <- list()
      for (i in seq_len(var_vfolds)) {
        aa <- tibble::as_tibble(purrr::transpose(test_classifiers_self[[i]]))
        aa <- dplyr::mutate_all(aa, as.numeric)
        aa <- dplyr::bind_cols(find_shapelets[[i]], aa) |>
          # dplyr::select(-data) |> ####### The final `model` we need is the shapelet
          dplyr::mutate(across(!where(is.list), as.numeric))

        sup_spec <- quantile(aa$specificity, 0.75, na.rm = TRUE)
        sup_prec <- quantile(aa$precision, 0.75, na.rm = TRUE)
        min_fp <- min(aa$fp, na.rm = TRUE)
        min_fn <- min(aa$fn, na.rm = TRUE)

        aa <- aa |>
          dplyr::filter(
            precision > sup_prec,
            specificity > sup_spec
          ) |>
          dplyr::arrange(fp, fn) |>
          dplyr::slice_head(n = 10)

        res[[i]] <- aa
      }
      res
    },
    pattern = map(test_classifiers_self, find_shapelets),
    iteration = "list"
  )
)

# tar_load(best_shapelets)

# readr::write_csv(best_shapelets[[1]], file = here::here("tmp", glue::glue("metricse.csv")))
