# Sys.setenv(TAR_PROJECT = "classifier")


# TODO: change pipeline to nested resamplng

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
var_signals_include <- "II"
# "time", "I", "II", "III", "V", "aVR", "aVL", "aVF", "PLETH", "ABP", "RESP", "MCL"
var_signals_exclude <- setdiff(const_signals, var_signals_include)

var_classes_include <- c("asystole", "bradycardia", "tachycardia", "fibv", "vtachy")

#### Pipeline: Start ----

r_input <- tar_files_input(
  #### Pipeline: Read files from directory ----
  file_paths,
  {
    find_all_files(classes = var_classes_include)
  }
)

r_dataset <- tar_target(
  #### Pipeline: Import the last 10 seconds of all TRUE and FALSE alarms ----
  dataset,
  {
    temp <- read_and_prepare_ecgs(file_paths,
      subset = var_subset # here we want all alarms
      # true_alarm = TRUE,
      # limit_per_class = var_limit_per_class
    )
    reshape_ds_by_truefalse(temp, var_signals_include)
  }
)

b_initial_split <- tar_target(
  #### Pipeline: Make the initial training/test split ----
  initial_split,
  {
    # the seed is binded to the target hash value, so this is reproductible
    # We could just apply initial_split over the `dataset`, but the `strata` argument
    # does not reach the `alarm` column that we use to stratify the sampling.
    # The stratification is done to keep classes, TRUEs and FALSEs proportional to the original dataset
    build_initial_split(dataset, prop = 0.75, strata = "class_alarm", signals = var_signals_include)
  }
)

# First draft, not following parsnip rules: https://tidymodels.github.io/model-implementation-principles/function-interfaces.html
b_fit_models <- tar_target(
  # values = list(serie = var_signals_include),
  # tar_target(
  #### Pipeline: Extract snippets for each class
  class_snippets,
  {
    # This first version will use only the TRUE and FALSE from the same class
    analysis_set <- rsample::analysis(initial_split[["II"]])
    windows_set <- seq(60, 500, by = 10) # size of the snippets, can be tuned for each class

    # which classes are present in the dataset?
    classes <- unique(analysis_set$class)

    snippets <- list()

    # do the thing for each class
    for (cl in classes) {
      cat("Starting Class ", cl, "\n")
      data_n <- analysis_set %>% dplyr::filter(alarm == "false")
      data_p <- analysis_set %>% dplyr::filter(class == cl, alarm == "true")

      neg_stream <- NULL

      for (i in seq_len(nrow(data_n))) {
        neg_stream <- c(neg_stream, (data_n$values[[i]]))
      }

      neg_stream_val <- neg_stream # validate_data(neg_stream, w)

      # cat("neg_stream validated, ", length(neg_stream_val), " length", "\n")

      pos_stream <- NULL

      for (i in seq_len(nrow(data_p))) {
        pos_stream <- c(pos_stream, (data_p$values[[i]]))
      }

      pos_stream_val <- pos_stream # validate_data(pos_stream, w)

      # cat("pos_stream validated, ", length(pos_stream_val), " length", "\n")

      pan_cp <- matrix(0, nrow = length(windows_set), ncol = length(pos_stream_val))

      for (w in seq_len(length(windows_set))) {
        cat("Starting contrast for window ", windows_set[w], "\n")
        con <- contrast(neg_stream_val, pos_stream_val, windows_set[w], progress = FALSE)$contrast_profile
        cat("Finished contrast for window ", windows_set[w], "\n")
        pan_cp[w, seq_along(con)] <- con
      }

      snippets[[cl]] <- pan_cp
    }

    snippets
  }
)

b_fit_models2 <- tar_target(
  try_classify,
  {
    analysis_set <- rsample::analysis(initial_split[["II"]])
    w <- seq(60, 500, by = 10) # size of the snippets, can be tuned for each class

    # which classes are present in the dataset?
    classes <- unique(analysis_set$class)

    for (cl in classes) {
      cat("Starting Class ", cl, "\n")
      data_class <- analysis_set %>% dplyr::filter(class == cl)

      for (i in seq_len(nrow(data_class))) {
        bsf_min <- Inf
        bsf_class <- NULL

        for (snip in classes) {
          query <- class_snippets[[snip]]$plato
          pre <- false.alarm::mass_pre(data_class$values[[i]], w, query)
          dp <- false.alarm::mass(pre, data_class$values[[i]], query)
          curr_min <- min(dp$distance_profile)

          if (bsf_min > curr_min) {
            bsf_min <- curr_min
            bsf_class <- snip
          }
        }

        cat("Class: ", cl, " Answer: ", bsf_class, "min: ", bsf_min, "\n")

        # # profile <- mpx(data_class$values[[i]], w, joint_snippets)
        # profile <- mpx(joint_snippets, w, data_class$values[[i]])
        # cat("MP: ", length(profile$matrix_profile), " Snip: ", length(joint_snippets), " Data: ", length(data_class$values[[i]]), "\n")
        # # cat("class ", cl, ": ", floor(profile$profile_index[which.min(profile$matrix_profile)] / w) + 1, "\n")
        # cat("class ", cl, ": ", floor(which.min(profile$matrix_profile) / w) + 1, "\n")
      }
    }
  }
)

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

b_fit_models_old <- tar_map(
  values = list(serie = var_signals_include),
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
