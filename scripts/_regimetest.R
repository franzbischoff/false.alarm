# Sys.setenv(TAR_PROJECT = "regime_change")

source(here::here("scripts", "_globals.R"), local = .GlobalEnv, encoding = "UTF-8")

# source(here("regimes", "tar_inner_resample.R"))
# source(here("regimes", "tar_outer_resample.R"))


options(target_ds_path = here("inst/extdata/afib_regimes"))
options(tidymodels.dark = TRUE)

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
var_subset <- 1:10000
var_limit_per_class <- 20

var_classes_include <- "paroxysmal_afib"
var_classes_exclude <- setdiff(const_classes, var_classes_include)

var_signals_include <- "II"
var_signals_exclude <- setdiff(const_signals, var_signals_include)

# window size of the filters used to remove spurious data
# var_filter_w_size <- 100 # c(100, 200)

# Matrix Profile:
## batch size of the online matrix profile algorithm (doesn't affect the results, only jumps snapshots for brevity)
var_mp_batch <- 100
## size of the online MP buffer to keep memory footprint low (multiply by the freq to have it in seconds)
var_mp_history <- 20 * const_sample_freq # 20 secs
## Threshold used on computing the MP. This concept is described elsewhere. Values above 10 use another formula
tune_var_mp_threshold <- c(0, 0.5) # c(0, 0.5, 0.6, 0.9, 50, 60, 90)
## the window size for the MP algorithm
tune_var_window_size <- c(200, 250)
## Multiplier for the size of the Exclusion Zone (e.g., window_size * ez == exclusion_zone)
var_ez <- 0.5
## time constraint used on MP. Don't use both mp_time_constraint and floss_time_constraint (multiply by the freq to have it in seconds)
tune_var_mp_time_constraint <- c(0, 5 * const_sample_freq) # , 17 * const_sample_freq)

# FLOSS:
## time constraint used on FLOSS. Don't use both mp_time_constraint and floss_time_constraint (multiply by the freq to have it in seconds)
tune_var_floss_time_constraint <- c(0, 5 * const_sample_freq) # , 17 * const_sample_freq)

# Regime change detection:
## Index position where the regime change algorithm will focus the attention
tune_var_regime_landmark <- c(2.5 * const_sample_freq, 3 * const_sample_freq) # 3 seconds from the end
## threshold below the regime change algorithm will trigger
tune_var_regime_threshold <- c(0.4, 0.5) # c(0.1, 0.2, 0.3, 0.4, 0.5)

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


############
# tuning variables
var_vfolds <- 10
var_vfolds_repeats <- 1
var_vfolds_groups <- 2 # for target branches
var_dopar_cores <- 4
var_window_size_tune <- c(150L, 350L)
var_mp_threshold_tune <- c(0, 0.9)
var_time_constraint_tune <- c(0L, 2500L)
var_regime_threshold_tune <- c(0.2, 0.6)
var_tune_bayes_iter <- 4
var_tune_bayes_initial <- 2
var_tune_bayes_no_improve <- 10
var_initial_split_prop <- 3 / 4


# # All configurations used different CPUs while running the code.
library(future)
library(future.callr)
# plan(multisession) # create top-level processes
# plan(multicore) # create child processes
plan(future.callr::callr) # create child processes with a child process

suppressPackageStartupMessages(library(tidymodels))
tidymodels::tidymodels_prefer(quiet = TRUE)

#### Pipeline: Start ----
# tune_var_mp_threshold <- c(0, 0.5)
# tune_var_window_size <- c(200, 250)
# tune_var_mp_time_constraint <- c(0, 5 * const_sample_freq)
# tune_var_floss_time_constraint <- c(0, 5 * const_sample_freq)
# tune_var_regime_landmark <- c(2.5 * const_sample_freq, 3 * const_sample_freq)
# tune_var_regime_threshold <- c(0.4, 0.5)

# start debugme after loading all functions
# if (dev_mode) {
debugme::debugme()
# }


list(
  tar_files_input(
    #### Pipeline: Read files from directory ----
    file_paths,
    find_all_files(here::here("inst/extdata/afib_regimes"),
      data_type = "regimes",
      classes = var_classes_include
      # limit_per_class = 10
    )
  ),
  tar_target(
    #### Pipeline: Import Files to R and Select Datasets ----
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
    #### Pipeline: Tidy dataset and create the initial resample ----
    initial_resample,
    {
      tidy_dataset <- purrr::map_dfr(dataset, function(x) {
        regimes <- attr(x, "regimes")
        tibble::tibble(truth = list(regimes), ts = list(x$II))
      }, .id = "id")
      rsample::initial_split(tidy_dataset, prop = var_initial_split_prop)
    }
  ),
  tar_target(
    #### Pipeline: Create the final testing split for the outer loop ----
    testing_split,
    { # outer loop, this will be evaluated last
      rsample::testing(initial_resample) # 5
    }
  ),
  tar_target(
    #### Pipeline: Create the training split for the inner loop ----
    training_split,
    { # outer-inner loop, this will be cross-validated
      rsample::training(initial_resample) # 15
    }
  ),
  tar_target(
    #### Pipeline: Subset the training split into analysis split (training) ----
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

      res <- rsample::manual_rset(this_split$splits, id = glue_fmt("Fold{seq_len(var_vfolds):02d}")) %>%
        dplyr::mutate(group = rep(seq_len(var_vfolds_groups),
          each = ceiling(var_vfolds / var_vfolds_groups),
          length.out = var_vfolds
        )) %>%
        dplyr::group_by(group) %>%
        tar_group()
      res
    },
    iteration = "group"
  ),
  tar_target(
    #### Pipeline: Subset the training split into assessment split (test) ----
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

      res <- rsample::manual_rset(this_split$splits, id = glue_fmt("Fold{seq_len(var_vfolds):02d}")) %>%
        dplyr::mutate(group = rep(seq_len(var_vfolds_groups),
          each = ceiling(var_vfolds / var_vfolds_groups),
          length.out = var_vfolds
        )) %>%
        dplyr::group_by(group) %>%
        tar_group()
      res
    },
    iteration = "group"
  ),
  tar_target(
    #### Pipeline: Here we will conduct the parameter optimizations ----
    analysis_fitted,
    {
      source(here::here("scripts", "regimes", "parsnip_model.R"), encoding = "UTF-8")

      # Fix for targets branches that wipes off these classes
      class(analysis_split) <- c("manual_rset", "rset", class(analysis_split))

      floss_mod <-
        floss_regime_model(
          window_size = tune::tune(),
          time_constraint = tune::tune(),
          mp_threshold = tune::tune(),
          regime_threshold = tune::tune()
        ) %>%
        parsnip::set_engine("floss") %>%
        parsnip::set_mode("regression")

      floss_set <- hardhat::extract_parameter_set_dials(floss_mod)
      floss_set <- floss_set %>% stats::update(
        window_size = window_size_par(var_window_size_tune),
        mp_threshold = mp_threshold_par(var_mp_threshold_tune),
        time_constraint = time_constraint_par(var_time_constraint_tune),
        regime_threshold = dials::threshold(var_regime_threshold_tune)
      )

      floss_rec <- recipes::recipe(x = head(analysis_split$splits[[1]]$data, 1)) %>%
        recipes::update_role(truth, new_role = "outcome") %>%
        recipes::update_role(id, new_role = "predictor") %>%
        recipes::update_role(ts, new_role = "predictor")

      # doMC::registerDoMC(cores = 8)
      doParallel::registerDoParallel(cores = var_dopar_cores)
      # tune bayes applies the same parameters to all resamples
      # if we want to tune the parameters for each resample, we need to
      # use the targets branches
      floss_search_res <- floss_mod %>%
        tune::tune_bayes(
          preprocessor = floss_rec,
          resamples = analysis_split,
          # To use non-default parameter ranges
          param_info = floss_set,
          # Generate five at semi-random to start
          initial = var_tune_bayes_initial,
          iter = var_tune_bayes_iter,
          # How to measure performance?
          metrics = yardstick::metric_set(floss_error), # help has the function signature
          control = tune::control_bayes(
            no_improve = var_tune_bayes_no_improve,
            verbose = TRUE, save_pred = TRUE, parallel_over = "resamples"
          )
        )
      floss_search_res <- clean_splits_data(floss_search_res)
      # doParallel::stopImplicitCluster()
      # class(analysis_fitted) <- c("iteration_results", "tune_results", "tbl_df", "tbl", "data.frame")
      floss_search_res
      # }
    },
    pattern = map(analysis_split),
    iteration = "list" # thus the objects keep their attributes
  ),
  tar_target(
    #### Pipeline: Here we select the best from each optimization split and test in a separate split ----
    analysis_evaluation,
    {
      all_fits <- lst_to_df(analysis_fitted)

      # default: For space-filling or random grids, a marginal effect plot is created.
      autoplot(all_fits, type = "marginals") + labs(title = "Marginal plot", x = "Parameter value", y = "Performance") +
        theme_bw()

      autoplot(all_fits, type = "parameters") + labs(title = "Parameter search", x = "Iterations", y = "Parameter value") +
        theme_bw()

      autoplot(all_fits, type = "performance", width = 0.1) + labs(title = "Performance - CI", x = "Iterations", y = "Performance") +
        theme_bw()

      best_model <- select_best(all_fits, metric = "floss_error")
      best_by_one_std_err <- select_by_one_std_err(all_fits, metric = "floss_error")
      best_by_pct_loss <- select_by_pct_loss(all_fits, metric = "floss_error", limit = 5)
      collect_metrics(all_fits, summarize = FALSE) %>%
        group_by(id) %>%
        summarise(mean = mean(.estimate), n = n(), std_err = sd(.estimate))

      fits_by_fold <- all_fits %>%
        select(id, .metrics) %>%
        unnest(.metrics) %>%
        select(-.estimator) %>%
        group_by(id) %>%
        arrange(.estimate) %>%
        group_map(~ head(.x, 3L), .keep = TRUE)
    },
    # pattern = map(analysis_fitted),
    iteration = "list" # thus the objects keep their attributes
  ),
  tar_target(
    #### Pipeline: In the end, get the best from the inner resample and test with the testing split ----
    testing_evaluation,
    {
      analysis_evaluation
      testing_split
    }
  )


  #### Pipeline: > NoFilters Branch ----

  # tar_map(
  #   ##### Pipeline: > NoFilters > WindowSize Branch ----
  #   values = list(map_window_size = tune_var_window_size),
  #   tar_target(
  #     ##### Pipeline: > NoFilters > WindowSize > Compute Stats ----
  #     ds_stats,
  #     {
  #       process_ts_in_file(dataset,
  #         id = "comp_stats",
  #         fun = compute_companion_stats,
  #         params = list(
  #           window_size = map_window_size,
  #           n_workers = 1
  #         ),
  #         exclude = var_signals_exclude
  #       )
  #     },
  #     pattern = map(dataset)
  #   ),
  #   tar_map(
  #     values = list(map_test = c(1, 2)),
  #     tar_target(
  #       test,
  #       list(sd = ds_stats, te = map_test),
  #       pattern = map(ds_stats)
  #     )
  #   )
  # )
)

# targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#   targets::tar_script({
#     set.seed(1)
#     nested <- rsample::nested_cv(tibble::as_tibble(1:10),
#       outside = rsample::vfold_cv(v = 5, repeats = 1),
#       inside = rsample::vfold_cv(v = 5, repeats = 1)
#     )

#     list(
#       tarchetypes::tar_map(
#         values = list(split = nested$splits, resample = nested$inner_resamples, names = rlang::syms(nested$id)),
#         targets::tar_target(x, list(split = split, res = resample)),
#         targets::tar_target(y, x, pattern = map(x)),
#         names = "names"
#       )
#     )
#   })
#   targets::tar_make()
#   targets::tar_manifest()
#   # targets::tar_load(x_ddf0b740)
# })

# ,
# tar_map(
#   #### Pipeline: > NoFilters > WindowSize > MP Threshold Branch ----
#   values = list(map_mp_threshold = tune_var_mp_threshold),
#   tar_map(
#     #### Pipeline: > NoFilters > WindowSize > MP Threshold > MP/FLOSS Constraints Branch ----
#     # here the expand_grid and filter do the following:
#     # -- first get all the combinations from the variables
#     # -- second, keep only the combinations where one (or both) sides are zero
#     # This will create a "common" branch where no constraint is applied and one branch with mp constraints and other with floss constraints.
#     # Seems complicated here, but has simplified the pipeline a lot.
#     values = dplyr::filter(tidyr::expand_grid(
#       map_floss_time_constraint = tune_var_floss_time_constraint,
#       map_mp_time_constraint = tune_var_mp_time_constraint
#     ), !(map_floss_time_constraint > 0 & map_mp_time_constraint > 0)),
#     tar_target(
#       ##### Pipeline: > NoFilters > WindowSize > MP Threshold > MP/FLOSS Constraints > Compute the Matrix Profile using Stats ----
#       ds_stats_mps,
#       process_ts_in_file(c(dataset, ds_stats),
#         id = "mps_raw_stats",
#         fun = compute_s_profile_with_stats,
#         params = list(
#           window_size = map_window_size,
#           ez = var_ez,
#           progress = FALSE,
#           batch = var_mp_batch,
#           history = var_mp_history,
#           mp_time_constraint = map_mp_time_constraint,
#           threshold = map_mp_threshold
#         ),
#         exclude = var_signals_exclude
#       ),
#       priority = 0.5,
#       pattern = map(dataset, ds_stats)
#     ),
#     tar_target(
#       ##### Pipeline: > NoFilters > WindowSize > MP Threshold > MP/FLOSS Constraints > Compute FLOSS ----
#       ds_stats_mps_floss,
#       process_ts_in_file(ds_stats_mps,
#         id = "floss_mps",
#         exclude = var_signals_exclude,
#         fun = compute_floss,
#         params = list(
#           window_size = map_window_size,
#           ez = round(var_ez * map_window_size + .Machine$double.eps^0.5),
#           mp_time_constraint = map_mp_time_constraint,
#           floss_time_constraint = map_floss_time_constraint,
#           history = var_mp_history,
#           sample_freq = const_sample_freq
#         )
#       ),
#       priority = 0.4,
#       pattern = map(ds_stats_mps)
#     ),
#     tar_map(
#       ##### Pipeline: > NoFilters > WindowSize > MP Threshold > MP/FLOSS Constraints > Regime Threshold Branch ----
#       values = list(map_regime_threshold = tune_var_regime_threshold),
#       tar_target(
#         ##### Pipeline: > NoFilters > WindowSize > MP Threshold > MP/FLOSS Constraints > Regime Threshold > Extract regime changes ----
#         regimes,
#         process_ts_in_file(ds_stats_mps_floss,
#           id = "regimes",
#           fun = extract_regimes,
#           params = list(
#             window_size = map_window_size,
#             ez = var_ez,
#             regime_threshold = map_regime_threshold,
#             regime_landmark = tune_var_regime_landmark, # 3 sec from the end
#             # progress = FALSE,
#             # batch = var_mp_batch,
#             history = var_mp_history,
#             mp_time_constraint = map_mp_time_constraint,
#             floss_time_constraint = map_floss_time_constraint
#           ),
#           exclude = var_signals_exclude
#         ),
#         pattern = map(ds_stats_mps_floss)
#       )
#     )
#   )
# )
