# region Do computation
script_files <- list.files(here::here("scripts", "common"), pattern = "*.R")
purrr::walk(here::here("scripts", "common", script_files), source, local = .GlobalEnv, encoding = "UTF-8")
rm(script_files)
source(here::here("scripts", "helpers", "glue_fmt.R"), local = .GlobalEnv, encoding = "UTF-8")
source(here::here("R", "floss_train.R"), local = .GlobalEnv, encoding = "UTF-8") # nolint
source(here::here("R", "floss_predict.R"), local = .GlobalEnv, encoding = "UTF-8") # nolint
source(here::here("scripts", "common", "score_floss.R"), local = .GlobalEnv, encoding = "UTF-8") # nolint

#########
# dataname <- "vtachyarrhythmias"
# # signal sample frequency, this is a constant
# const_sample_freq <- 250
# const_signals <- c("time", "ECG")

# var_resample_from <- 0
# var_resample_to <- 0

# var_subset <- NULL # 1:10000 # NULL
# var_limit_per_class <- NULL

# var_classes_include <- NULL
# var_classes_exclude <- NULL

# var_signals_include <- "ECG"
# var_signals_exclude <- setdiff(const_signals, var_signals_include)
# #########

# dataname <- "malignantventricular"
# # signal sample frequency, this is a constant
# const_sample_freq <- 250
# const_signals <- c("time", "ECG1")

# var_resample_from <- 0
# var_resample_to <- 0

# var_subset <- NULL # 1:10000 # NULL
# var_limit_per_class <- NULL

# var_classes_include <- NULL
# var_classes_exclude <- NULL

# var_signals_include <- "ECG1"
# var_signals_exclude <- setdiff(const_signals, var_signals_include)
#########
dataname <- "afib_regimes"

const_sample_freq <- 250
const_signals <- c("time", "I", "II")
const_classes <- c("persistent_afib", "paroxysmal_afib", "non_afib")

var_resample_from <- 200
var_resample_to <- const_sample_freq

var_subset <- NULL # 1:10000
var_limit_per_class <- NULL

var_classes_include <- "paroxysmal_afib"
var_classes_exclude <- setdiff(const_classes, var_classes_include)

var_signals_include <- "II"
var_signals_exclude <- setdiff(const_signals, var_signals_include)
########

file_paths <- find_all_files(here::here("inst", "extdata", dataname),
  data_type = "regimes",
  classes = var_classes_include
  # limit_per_class = 10
)

# 2 - Read and prepare the data
dataset <- read_and_prepare_ecgs(file_paths,
  subset = var_subset,
  limit_per_class = var_limit_per_class,
  data_type = "regime",
  resample_from = var_resample_from,
  resample_to = var_resample_to,
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

# ensure truth values are clean
for (i in seq_along(tidy_dataset$truth)) {
  tidy_dataset$truth[[i]] <- clean_truth(tidy_dataset$truth[[i]], length(tidy_dataset$ts[[i]]))
}

rm(dataset)

#############
var_window_size <- seq(350, 400, by = 25)
split <- 5
#############

tic <- Sys.time()
# Set up parallel processing
future::plan(future::multicore) # Use multiple sessions for parallel processing
# Parallelized computation of floss
# ## This computes the floss of the given Time Series
# floss_dataset will be huge, needs memory or split var_window_size
floss_dataset <- NULL
for (w in var_window_size) {
  cli::cli_inform(c("i" = "Starting processing window size {w}"))
  tictic <- Sys.time()
  floss <- furrr::future_map(tidy_dataset$ts, ~ {
    floss_train_regimes(.x, w, 0, 0)
  }, .options = furrr::furrr_options(seed = NULL))
  checkmate::qassert(floss, "L+")
  floss_dataset <- dplyr::bind_rows(
    floss_dataset,
    tibble::add_column(tidy_dataset, window_size = w, floss = floss) |>
      dplyr::select(-ts)
  )
  tactac <- Sys.time()
  cli::cli_inform(c("v" = "Finished processing window size {w} in {round(difftime(tactac, tictic, units = 'mins'), 2)} minutes"))
}
tac <- Sys.time()
cli::cli_inform(c("!" = "Time taken to compute floss: {round(difftime(tac, tic, units = 'mins'), 2)} minutes"))

rm(tidy_dataset)

var_regime_threshold <- seq(0.05, 0.9, by = 0.05)
var_regime_landmark <- seq(2, 9, by = 0.5)
# create a grid of all combinations of the parameters
grid <- expand.grid(
  regime_threshold = var_regime_threshold,
  regime_landmark = var_regime_landmark
)

tic <- Sys.time()
## This computes the regime changes of the given floss
floss_preds <- purrr::map_dfr(seq_len(nrow(floss_dataset)), function(i) {
  cli::cli_inform(c("i" = "Starting processing row {i} of {nrow(floss_dataset)}"))
  current_row <- floss_dataset[i, ]

  tictic <- Sys.time()
  pred_dataset <- purrr::map_dfr(seq_len(nrow(grid)), function(j) {
    rt <- grid$regime_threshold[j]
    rl <- grid$regime_landmark[j]

    pred <- floss_predict(
      current_row$floss[[1]],
      current_row$window_size,
      0,
      rt,
      rl
    ) |> clean_pred(200, TRUE)

    if (is.null(pred)) {
      cli::cli_inform(c("!" = "No prediction for row {i} and grid {j}"))
    }

    current_row |>
      dplyr::select(-floss) |>
      dplyr::mutate(
        regime_threshold = rt,
        regime_landmark = rl,
        pred = list(pred)
      )
  })

  tactac <- Sys.time()
  cli::cli_inform(c("v" = "Finished processing row {i} in {round(difftime(tactac, tictic, units = 'mins'), 2)} minutes"))
  pred_dataset
})
tac <- Sys.time()
cli::cli_inform(c("!" = "Time taken to compute predictions: {round(difftime(tac, tic, units = 'mins'), 2)} minutes"))

tic <- Sys.time()
floss_scores <- list()
for (i in seq_len(nrow(floss_preds))) {
  score <- score_pr(floss_preds$truth[[i]], floss_preds$pred[[i]], const_sample_freq, 10, 4)
  floss_scores[[i]] <- 1 - score
}

final_dataset <- tibble::add_column(floss_preds, score = unlist(floss_scores))
final_dataset$regime_threshold <- round(final_dataset$regime_threshold, 2)
final_dataset$regime_landmark <- round(final_dataset$regime_landmark, 1)
final_dataset$window_size <- as.integer(final_dataset$window_size)
tac <- Sys.time()
cli::cli_inform(c("!" = "Time taken to compute scores: {round(difftime(tac, tic, units = 'mins'), 2)} minutes"))

final_dataset <- final_dataset |> dplyr::rename(record = id)
saveRDS(final_dataset, file = here::here("output", glue::glue("{dataname}-{split}.rds")), compress = "xz")

# final_dataset <- readRDS(here::here("output", glue::glue("{dataname}-1.rds")))
# endregion Do computation
