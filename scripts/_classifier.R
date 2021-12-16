# Sys.setenv(TAR_PROJECT = "classifier")

# Load global config
source(here("scripts", "_globals.R"))
# Load all scripts
script_files <- list.files(here::here("scripts", "classification"), pattern = "*.R")
sapply(here::here("scripts", "classification", script_files), source, local = .GlobalEnv, encoding = "UTF-8")
rm(script_files)

# Overwrite some configs
options(tidymodels.dark = TRUE)

tar_option_set(
  packages = c("discrim", "yardstick", "tidymodels", "dplyr", "false.alarm")
)

# The subset that will be keep from the dataset (seq.int(290 * const_sample_freq + 1, 300 * const_sample_freq) means the last 10 seconds)
var_subset <- seq.int(290 * const_sample_freq + 1, 300 * const_sample_freq) # last 10 secs
var_include <- "II"
# "time", "I", "II", "III", "V", "aVR", "aVL", "aVF", "PLETH", "ABP", "RESP", "MCL"
var_exclude <- setdiff(const_headers, var_include)

#### Pipeline: Start ----

r_input <- tar_files_input(
  #### Pipeline: Read files from directory ----
  file_paths,
  {
    find_all_files(types = "asystole") # types = "asystole", "bradycardia", "tachycardia", "vfib", "vtachy"
  }
)

r_dataset <- tar_target(
  #### Pipeline: Import the last 10 seconds of all TRUE and FALSE alarms ----
  dataset,
  {
    temp <- read_and_prepare_ecgs(file_paths,
      subset = var_subset # here we want all alarms
      # true_alarm = TRUE,
      # limit_per_type = var_limit_per_type
    )
    reshape_dataset_by_truefalse(temp, var_include)
  }
)

b_initial_split <- tar_target(
  #### Pipeline: Make the initial training/test split ----
  initial_split,
  {
    # the seed is binded to the target hash value, so this is reproductible
    # We could just apply initial_split over the `dataset`, but the `strata` argument
    # does not reach the `alarm` column that we use to stratify the sampling.
    # The stratification is done to keep TRUEs and FALSEs proportional to the original dataset
    build_initial_split(dataset, prop = 0.75, strata = "alarm", include = var_include)
  }
)

b_fit_models <- tar_map(
  values = list(serie = var_include),
  tar_target(
    #### Pipeline: Fit models over a cross validation framework using the training set ----
    # more methods: https://rsample.tidymodels.org/reference/index.html#section-resampling-methods
    fitted_models,
    fit_models(initial_split[[serie]],
      strata = "alarm",
      arguments = list(
        resampling = rsample::vfold_cv,
        folds = 5,
        repeats = 3,
        model = parsnip::mlp,
        tune = 10,
        formula = as.formula(alarm ~ val),
        metric = yardstick::metric_set(yardstick::roc_auc) # https://yardstick.tidymodels.org/articles/metric-types.html
      )
    )
  ),
  tar_target(
    #### Pipeline: Fit the best model and test on the test set ----
    fit_best_model,
    {
      # Get the best model based on some metric: https://yardstick.tidymodels.org/articles/metric-types.html
      best_model <- tune::select_best(fitted_models, "roc_auc")
      # hack to get the model_spec and formula from the last target
      wf <- attr(fitted_models, "workflow")
      model <- tune::extract_spec_parsnip(wf)
      formula <- tune::extract_preprocessor(wf)

      # update the model_spec with the best parameters
      model <- model %>% tune::finalize_model(best_model)

      # last_fit() emulates the process where, after determining the best model, the final fit
      # on the entire training set is needed and is then evaluated on the test set.
      final_fit <- model %>% tune::last_fit(initial_split[[serie]], preprocessor = formula)

      # metrics <- tune::collect_metrics(final_fit)
      final_fit
    }
  )
)

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
list(r_input, r_dataset, b_initial_split, b_fit_models)

#### Pipeline: End ----
