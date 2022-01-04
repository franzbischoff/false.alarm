# Sys.setenv(TAR_PROJECT = "classifier")

# Erros: data_val_pos_neg_150_II
# data_val_pos_neg_150_PLETH, data_val_pos_neg_300_II, data_val_pos_neg_300_PLETH
# longer object length is not a multiple of shorter object length

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
var_signals_include <- c("II", "PLETH")
var_bool_include_all <- TRUE
# "time", "I", "II", "III", "V", "aVR", "aVL", "aVF", "PLETH", "ABP", "RESP", "MCL"
var_signals_exclude <- setdiff(const_signals, var_signals_include)
var_classes_include <- c("asystole", "bradycardia", "tachycardia", "fibv", "vtachy")
var_snippet_size <- c(150, 300)
var_n_workers <- 1
# var_limit_per_class <- 15

#### Pipeline: Start ----
list(
  r_input <- tar_files_input(
    #### Pipeline: Read files from directory ----
    file_paths,
    {
      find_all_files(classes = var_classes_include)
    }
  ),
  r_dataset <- tar_target(
    #### Pipeline: Import the last 10 seconds of all TRUE and FALSE alarms ----
    dataset,
    {
      temp <- read_and_prepare_ecgs(file_paths,
        subset = var_subset
        # true_alarm = TRUE,  # here we want all alarms
        # limit_per_class = var_limit_per_class
      )
      reshape_ds_by_truefalse(temp, var_signals_include)
    }
  ),
  b_initial_split <- tar_target(
    #### Pipeline: Make the initial training/test split ----
    ds_initial_split,
    {
      # the seed is binded to the target hash value, so this is reproductible
      # We could just apply initial_split over the `dataset`, but the `strata` argument
      # does not accept more than one variable, so we need a dummy called "class_alarm" to stratify the sampling.
      # The stratification is done to keep classes, TRUEs and FALSEs proportional to the original dataset
      build_initial_split(dataset, prop = 0.95, strata = "class_alarm", signals = var_signals_include)
    }
  ),
  tar_map(
    values = list(map_signals_include = var_signals_include),
    tar_map(
      values = list(map_snippet_size = var_snippet_size),
      # First draft, not following parsnip rules: https://tidymodels.github.io/model-implementation-principles/function-interfaces.html
      b_data_pos_neg <- tar_target(
        #### Pipeline: Build the positive and negative streams, without signal validation, compute the positive MP
        data_pos_neg,
        build_pos_neg(ds_initial_split,
          signal = map_signals_include,
          snippet_size = map_snippet_size,
          validate = FALSE,
          same_class = TRUE
        )
      ),
      b_data_val_pos_neg <- tar_target(
        #### Pipeline: Build the positive and negative streams, with signal validation, compute the positive MP
        data_val_pos_neg,
        build_pos_neg(ds_initial_split,
          signal = map_signals_include,
          snippet_size = map_snippet_size,
          validate = TRUE,
          same_class = TRUE
        )
      ),
      b_find_snippets <- tar_target(
        #### Pipeline: Computes the AA - AB difference.
        pos_snippets,
        find_snippets(data_pos_neg,
          signal = map_signals_include,
          reverse = FALSE
        )
      ),
      b_find_val_snippets <- tar_target(
        #### Pipeline: Computes the AA - AB difference.
        pos_val_snippets,
        find_snippets(data_val_pos_neg,
          signal = map_signals_include,
          reverse = FALSE
        )
      )
      # b_find_neg_snippets <- tar_target(
      #   #### Pipeline: Computes the AA - BA difference.
      #   neg_snippets,
      #   find_snippets(data_pos_neg,
      #     signal = map_signals_include,
      #     reverse = TRUE
      #   )
      # ),
      # b_find_neg_val_snippets <- tar_target(
      #   #### Pipeline: Computes the AA - BA difference.
      #   neg_val_snippets,
      #   find_snippets(data_val_pos_neg,
      #     signal = map_signals_include,
      #     reverse = TRUE
      #   )
      # )
    )
  )
)

# b_fit_models2 <- tar_target(
#   try_classify,
#   {
#     analysis_set <- rsample::analysis(initial_split[["II"]])
#     w <- seq(60, 500, by = 10) # size of the snippets, can be tuned for each class

#     # which classes are present in the dataset?
#     classes <- unique(analysis_set$class)

#     for (cl in classes) {
#       cat("Starting Class ", cl, "\n")
#       data_class <- analysis_set %>% dplyr::filter(class == cl)

#       for (i in seq_len(nrow(data_class))) {
#         bsf_min <- Inf
#         bsf_class <- NULL

#         for (snip in classes) {
#           query <- class_snippets[[snip]]$plato
#           pre <- false.alarm::mass_pre(data_class$values[[i]], w, query)
#           dp <- false.alarm::mass(pre, data_class$values[[i]], query)
#           curr_min <- min(dp$distance_profile)

#           if (bsf_min > curr_min) {
#             bsf_min <- curr_min
#             bsf_class <- snip
#           }
#         }

#         cat("Class: ", cl, " Answer: ", bsf_class, "min: ", bsf_min, "\n")

#         # # profile <- mpx(data_class$values[[i]], w, joint_snippets)
#         # profile <- mpx(joint_snippets, w, data_class$values[[i]])
#         # cat("MP: ", length(profile$matrix_profile), " Snip: ", length(joint_snippets), " Data: ", length(data_class$values[[i]]), "\n")
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
# list(r_input, r_dataset, b_initial_split, b_data_pos_neg, b_find_snippets, b_find_neg_snippets, b_data_val_pos_neg, b_find_val_snippets, b_find_neg_val_snippets)

#### Pipeline: End ----
