# Sys.setenv(TAR_PROJECT = "classifier")
# source("renv/activate.R")
# Errors: data_pos_neg_150_II
# data_pos_neg_150_PLETH, data_pos_neg_300_II, data_pos_neg_300_PLETH
# longer object length is not a multiple of shorter object length

library(targets)
library(tarchetypes)


# Load global config
# source(here::here("scripts", "_globals.R"), local = .GlobalEnv, encoding = "UTF-8") # nolint
script_files <- list.files(here::here("scripts", "common"), pattern = "*.R")
purrr::walk(here::here("scripts", "common", script_files), source, local = .GlobalEnv, encoding = "UTF-8")
rm(script_files)
source(here::here("scripts", "helpers", "glue_fmt.R"), local = .GlobalEnv, encoding = "UTF-8")

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

# controller <- crew::crew_controller_local(
#   name = "my_controller",
#   workers = 10,
#   seconds_idle = 3
# )

# var_shapelet_size <- c(120, 300, 1000) # c(30, 60, 120, 180, 300) # c(150, 300)
var_shapelet_sizes <- get_exp_dist_series(20, 400, 20) # (20, 400, 20) # c(20, 60, 100, 140, 180, 220, 260, 300)
var_positive <- TRUE # c(TRUE, FALSE)
var_num_shapelets <- 10
var_num_neighbors <- 10
var_min_corr_neighbors <- 0.85

############
# tuning variables
# splits
# initial split, 3/4 will hold 25% of the data for final, independent, performance.
var_initial_split_prop <- 3 / 4
var_vfolds <- 5 # for the inner resample
var_vfolds_repeats <- 2 # for the inner resample
# parallel
var_dopar_cores <- 2 # number of cores to use on tuning (inner resample)
var_future_workers <- 2

var_verbose <- TRUE
var_save_workflow <- FALSE
var_save_pred <- TRUE

# options(
#   future.batchtools.output = TRUE, # future.cache.path = here::here(".cache"),
#   future.delete = FALSE
# ) # nolint




# tidymodels::tidymodels_prefer(quiet = TRUE)

# fx = function(x) {
#     x |> dplyr::mutate(area = Sepal.Length * Sepal.Width) |> head()
# }


# ssh -o "ExitOnForwardFailure yes" -f \
#     -R 57109:localhost:6607 \
#     -R 57110:localhost:6608 \
#     root@claster \
#     "R --no-save --no-restore -e \
#         'clustermq:::ssh_proxy(ctl=57109, job=57110)' \
#         > ~/test.log 2>&1"


tar_option_set(
  tidy_eval = TRUE,

  # library = "/workspace/.cache/R/renv/proj_libs/false.alarm-d6f1a0d1/R-4.3/x86_64-pc-linux-gnu",
  packages = c(
    "here", "rlang", "glue", "cli", "false.alarm", "dplyr", "rsample", "tidyr", "tibble", "purrr"
  ),
  envir = globalenv(),
  workspace_on_error = TRUE, # "contrast_profiles",
  format = "rds",
  memory = "transient",
  # debug = "dataset"
  garbage_collection = TRUE
)



# start debugme after loading all functions
# if (dev_mode) {
# debugme::debugme()
# }


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

      # aa <- round(runif(50, 1, 331))
      # res$II <- res$II[aa, ]
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
      "!DEBUG Starting contrast_profiles"

      # source(here::here("remote.R"))

      # cat(str(targets::tar_option_get("packages")), "\n")

      shapelet_sizes <- var_shapelet_sizes

      class(analysis_split) <- c("manual_rset", "rset", class(analysis_split))

      res <- list()
      for (i in seq_len(var_vfolds)) {
        fold <- rsample::get_rsplit(analysis_split, i)
        res[[i]] <- contrastprofile_topk(fold, shapelet_sizes, var_num_shapelets, n_jobs = 1, TRUE)
      }

      res
    },
    pattern = map(analysis_split),
    iteration = "list"
  ),
  tar_target(
    #### Pipeline: extract_metadata - Preparation of the data: the model's data is the shapelets with metadata ----
    extract_metadata,
    {
      res <- list()
      for (i in seq_len(var_vfolds)) {
        cli::cli_alert_info("Extracting metadata, fold {i}.")
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
    #### Pipeline: combine_shapelets - This is the model fit. ----
    combine_shapelets,
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
        solutions <- find_solutions(extract_metadata[[i]],
          min_cov = 10,
          max_shapelets = 20, # this can be more than topk
          rep = 10000,
          max_red = 10,
          max_k = tune3,
          n_jobs = var_future_workers
        )

        if (length(solutions) == 0) {
          res[i] <- list(NULL)
        } else {
          res[[i]] <- solutions
        }
      }
      res
    },
    pattern = map(extract_metadata),
    iteration = "list"
  ),
  tar_target(
    #### Pipeline: self_optimize_classifier - This is the current score function. ----
    self_optimize_classifier,
    {
      # With the results of this step, plus the fitted solutions, we need to find which
      # metadata is the best to filter the solutions
      class(analysis_split) <- c("manual_rset", "rset", class(analysis_split))

      res <- list()
      for (i in seq_len(var_vfolds)) {
        fold <- rsample::get_rsplit(analysis_split, i)
        shapelets <- combine_shapelets[[i]]

        # the `compute_metrics_topk` function may need testing on the `TRUE` criteria
        # currently, if `ANY` shapelet matches, it is considered a positive
        # as alternative we can try to use `ALL`, `HALF` or other criteria

        training_metrics <- compute_metrics_topk(fold, shapelets, 5, TRUE)

        res[[i]] <- list(training_metrics = training_metrics, shapelets = shapelets)
      }

      res # list(fold = res, overall = overall)


      # aa <- tibble::as_tibble(purrr::transpose(self_optimize_classifier[[1]][[i]]))
      # aa <- dplyr::mutate_all(aa, as.numeric)
      # aa <- dplyr::bind_cols(combine_shapelets[[1]][[i]], aa) |>
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
    pattern = map(combine_shapelets, analysis_split),
    iteration = "list"
  ),
  tar_target(
    test_classifier,
    {
      # Here we test the solutions we chose on the assessment split
      # The final `model` we need is the shapelet
      class(assessment_split) <- c("manual_rset", "rset", class(assessment_split))

      res <- list()

      for (i in seq_len(var_vfolds)) {
        fold <- rsample::get_rsplit(assessment_split, i)

        best_shapelets <- combine_metrics(
          self_optimize_classifier[[i]]$training_metrics,
          self_optimize_classifier[[i]]$shapelets
        )

        bb <- compute_metrics_topk(fold, best_shapelets, var_future_workers, TRUE)
        bb <- list_dfr(bb)
        aa <- best_shapelets |> dplyr::select(tp:random)
        metadata <- best_shapelets |>
          dplyr::select(c_total:data)

        res[[i]] <- list(aa = aa, bb = bb, metadata = metadata) # dplyr::slice_head(metrics, n = 3)
      }
      # overall <- compute_overall_metric(res)
      # list(fold = res, overall = overall)
      list(res = res)
    },
    pattern = map(self_optimize_classifier, assessment_split),
    iteration = "list"
  ),
  tar_target(
    test_classifier_metrics,
    {
      res <- list()
      for (i in seq_len(var_vfolds)) {
        fold <- test_classifier$res[[i]]
        namesmeta <- names(fold$metadata)
        cc <- tibble::as_tibble(fold$aa - fold$bb)
        namecols <- names(fold$aa)
        namecolsa <- glue::glue("{namecols}_aa")
        namecolsb <- glue::glue("{namecols}_bb")
        colnames(fold$aa) <- namecolsa
        colnames(fold$bb) <- namecolsb
        cc <- dplyr::bind_cols(cc, fold$aa, fold$bb)
        cc <- cc %>% dplyr::select(sort(names(.)))
        cc <- cc |> dplyr::relocate(tp, tp_aa, tp_bb, fp, fp_aa,
          fp_bb, tn, tn_aa, tn_bb, fn, fn_aa, fn_bb,
          .before = 1
        )
        cc <- dplyr::bind_cols(cc, fold$metadata)
        metrics <- cc |>
          # dplyr::filter(abs(precision) < rrank(precision, 2, 3)) |>
          # dplyr::filter(abs(km) < rrank(km, 2, 3)) |>
          dplyr::arrange(
            dplyr::desc(cov_mean) # dplyr::desc(specificity_bb),
            # dplyr::desc(precision_bb), # dplyr::desc(specificity_bb),
            # dplyr::desc(km_bb) # , fp_bb, fn_bb
          )
        metrics <- metrics |>
          dplyr::select(c(all_of(namecolsb), all_of(namesmeta))) |>
          dplyr::rename_with(~ gsub("_bb", "", .x, fixed = TRUE))

        res[[i]] <- metrics # dplyr::slice_head(metrics, n = 13)
      }
      list(fold = res)
    },
    pattern = map(test_classifier),
    iteration = "list"
  ),
  tar_target(
    #### Pipeline: test_holdout - This is the current score function. ----
    test_holdout,
    {
      # With the results of this step, plus the fitted solutions, we need to find which # < c_total, < red,  > km > orec
      # metadata is the best to filter the solutions

      class_metric <- list(metrics = test_classifier_metrics)

      metrics <- NULL
      for (i in seq_len(var_vfolds_repeats)) {
        for (j in seq_len(var_vfolds)) {
          class_metric$metrics[[i]]$fold[[j]] <- dplyr::mutate(class_metric$metrics[[i]]$fold[[j]], fold = j)
        }
        mrow <- list_dfr(class_metric$metrics[[i]]$fold) |> dplyr::mutate(repeats = i)
        metrics <- dplyr::bind_rows(metrics, mrow)
      }

      metrics <- metrics |> dplyr::mutate(idx = dplyr::row_number(), model = glue::glue("{coverage}_{redundancy}_{samples}"))

      models <- metrics |>
        dplyr::group_by(model) |>
        dplyr::summarize(across(precision:kappa, ~ mean(.x, na.rm = TRUE)),
          reps = length(unique(repeats)),
          folds = length(unique(fold)), n = length(fold)
        ) |>
        dplyr::filter(reps > 1, folds > 1, km > 0) |>
        dplyr::arrange(dplyr::desc(precision))

      mod <- metrics |> dplyr::filter(model == models$model[1])

      fold <- list(data = testing_split)
      shapelets <- mod |>
        dplyr::arrange(
          dplyr::desc(precision)
        ) |>
        dplyr::slice_head(n = 5)

      # the `compute_metrics_topk` function may need testing on the `TRUE` criteria
      # currently, if `ANY` shapelet matches, it is considered a positive
      # as alternative we can try to use `ALL`, `HALF` or other criteria
      metric <- list_dfr(compute_metrics_topk(fold, shapelets, var_future_workers, TRUE))

      overall <- compute_overall_metric_range(metric)

      res <- list(model = shapelets, metric = metric, overall = overall)

      plots <- list()
      for (i in seq_len(nrow(res$model))) {
        plots[[i]] <- plot_holdout_models(res, i)
      }
      res[["plots"]] <- plots
      res
    }
  )
)

# plt <- patchwork::wrap_plots(test_holdout$plots, ncol = 1) +
#   patchwork::plot_annotation(
#     title = "branch_name",
#     theme = ggplot2::theme(plot.title = ggplot2::element_text(family = "Roboto"))
#   )
# print(plt)

# tar_load(best_shapelets)

# readr::write_csv(best_shapelets[[1]], file = here::here("tmp", glue::glue("metricse.csv")))
