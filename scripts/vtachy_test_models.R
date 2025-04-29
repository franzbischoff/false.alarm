# This file is a complement of the Target _regime_change2.R
# The purpose is to compute the models for the remaining files of the outer loop
# Since there will be no outer loop, but only a 5-fold cross-validation

Sys.setenv(TAR_PROJECT = "regime_change2")
source(here::here("scripts", "_globals.R"), local = .GlobalEnv, encoding = "UTF-8") # nolint

options(target_ds_path = here::here("inst", "extdata", "vtachyarrhythmias")) # nolint
options(tidymodels.dark = TRUE) # nolint
options(progressr.enable = TRUE) # nolint

# signal sample frequency, this is a constant
const_sample_freq <- 250
const_signals <- c("time", "ECG")

var_subset <- NULL # 1:10000 # NULL
var_limit_per_class <- NULL

var_signals_include <- "ECG"
var_signals_exclude <- setdiff(const_signals, var_signals_include)

# initial split, 3/4 will hold 25% of the data for final, independent, performance.
var_initial_split_prop <- 3 / 4
var_vfolds <- 5 # for the inner resample
var_vfolds_repeats <- 2 # for the inner resample

# region Compare real data with the targets
# 1 - List all files in the vtachyarrhythmias folder
file_paths <- find_all_files(here::here("inst", "extdata", "vtachyarrhythmias"),
  data_type = "regimes"
  # classes = NULL
  # limit_per_class = 10
)
test <- tar_read("file_paths")
waldo::compare(file_paths, test)

# 2 - Read and prepare the data
dataset <- read_and_prepare_ecgs(file_paths,
  subset = var_subset,
  limit_per_class = var_limit_per_class,
  data_type = "regime",
  # resample_from = var_resample_from,
  # resample_to = var_resample_to,
  normalize = TRUE
)
test <- tar_read("dataset")
waldo::compare(dataset, test)

# 3 - Create the target variable
# set.seed(tar_meta(initial_resample, seed)$seed)
set.seed(-972821040)
tidy_dataset <- purrr::map_dfr(dataset, function(x) {
  regimes <- attr(x, "regimes")
  if (length(regimes) == 0) {
    return(NULL) # remove files that has no change in the subset
  }
  if (length(regimes) == 1 && regimes == 0) {
    return(NULL) # remove files that has no change in the subset
  }
  tibble::tibble(truth = list(regimes), ts = list(x[[var_signals_include]]))
}, .id = "id")
initial_resample <- rsample::initial_split(tidy_dataset, prop = var_initial_split_prop)
test <- tar_read("initial_resample")
waldo::compare(initial_resample, test)

# 4- outer loop, this will be evaluated last
set.seed(184895865)
testing_split <- rsample::testing(initial_resample)
test <- tar_read("testing_split")
waldo::compare(testing_split, test)

# 5 - outer-inner loop, this will be cross-validated
set.seed(708679313)
training_split <- rsample::training(initial_resample)
test <- tar_read("training_split")
waldo::compare(training_split, test)


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
analysis_split <- result |>
  dplyr::group_by(rep) |>
  tar_group()
test <- tar_read("analysis_split")
waldo::compare(analysis_split, test)


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
assessment_split <- result |>
  dplyr::group_by(rep) |>
  tar_group()
test <- tar_read("assessment_split")
waldo::compare(assessment_split, test)
# endregion Compare real data with the targets

# region Do computation
source(here::here("R", "floss_predict.R"), local = .GlobalEnv, encoding = "UTF-8") # nolint
source(here::here("scripts", "common", "score_floss.R"), local = .GlobalEnv, encoding = "UTF-8") # nolint

file_paths <- find_all_files(here::here("inst", "extdata", "vtachyarrhythmias"),
  data_type = "regimes"
  # classes = NULL
  # limit_per_class = 10
)

# 2 - Read and prepare the data
dataset <- read_and_prepare_ecgs(file_paths,
  subset = var_subset,
  limit_per_class = var_limit_per_class,
  data_type = "regime",
  # resample_from = var_resample_from,
  # resample_to = var_resample_to,
  normalize = TRUE
)

tidy_dataset <- purrr::map_dfr(dataset, function(x) {
  regimes <- attr(x, "regimes")
  if (length(regimes) == 0) {
    return(NULL) # remove files that has no change in the subset
  }
  if (length(regimes) == 1 && regimes == 0) {
    return(NULL) # remove files that has no change in the subset
  }
  tibble::tibble(truth = list(regimes), ts = list(x[[var_signals_include]]))
}, .id = "id")


# Set up parallel processing
future::plan(multisession) # Use multiple sessions for parallel processing
# Parallelized computation of floss
# ## This computes the floss of the given Time Series
progressr::with_progress({
  p <- progressr::progressor(steps = 2 * length(tidy_dataset$ts))
  floss <- furrr::future_map(tidy_dataset$ts, ~ {
    p()
    floss_train_regimes(.x, 100, 0, 0)
  }, .options = furrr::furrr_options(seed = NULL))
})

floss_truths <- list()
for (i in seq_along(tidy_dataset$truth)) {
  # ensure truth values are clean
  floss_truths[[i]] <- clean_truth(tidy_dataset$truth[[i]], length(tidy_dataset$ts[[i]]))
}
## This computes the regime changes of the given floss
floss_preds <- list()
for (i in seq_along(tidy_dataset$ts)) {
  pred <- floss_predict(floss[[i]], 100, 0, 0.5, 3)
  # ensure predicted values are clean
  floss_preds[[i]] <- clean_pred(pred, 200, TRUE)
}

floss_scores <- list()
for (i in seq_along(tidy_dataset$ts)) {
  score <- score_pr(floss_truths[[i]], floss_preds[[i]], 250, 10, 4)
  floss_scores[[i]] <- 1 - score
}

tibble_dataframe <- tibble::tibble(
  id = tidy_dataset$id,
  window_size = 100,
  regime_threshold = 0,
  regime_landmark = 0.5,
  score = floss_scores
)
# endregion Do computation
