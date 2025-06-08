# region
# script_files <- list.files(here::here("scripts", "common"), pattern = "*.R")
# purrr::walk(here::here("scripts", "common", script_files), source, local = .GlobalEnv, encoding = "UTF-8")
# rm(script_files)
# source(here::here("scripts", "helpers", "glue_fmt.R"), local = .GlobalEnv, encoding = "UTF-8")
# source(here::here("R", "floss_train.R"), local = .GlobalEnv, encoding = "UTF-8") # nolint
# source(here::here("R", "floss_predict.R"), local = .GlobalEnv, encoding = "UTF-8") # nolint
# source(here::here("scripts", "common", "score_floss.R"), local = .GlobalEnv, encoding = "UTF-8") # nolint

# "afib_regimes", "vtachyarrhythmias", "malignantventricular"
dataname <- "afib_regimes"

data <- readRDS(here("output", glue("{dataname}.rds")))

file_names <- sort(unique(data$record))
# get the count of total file names
total_files <- length(file_names)
# number of folds
n_folds <- 5
# create folds
folds <- cut(seq(1, total_files), breaks = n_folds, labels = FALSE)
# shuffle the folds
set.seed(42)
folds <- sample(folds)

predictors_names <- c("regime_threshold", "window_size", "regime_landmark")

# group by the predictors columns and create a new column with value of each predictor combination
data_grouped <- data |>
  dplyr::group_by(dplyr::across(dplyr::all_of(predictors_names))) |>
  dplyr::mutate(model = dplyr::cur_group_id())

data_grouped$id <- (sprintf("%04d", data_grouped$model))
data_grouped$id_text <- (sprintf("Model_%04d", data_grouped$model))
# data_grouped$record <- gsub("cu(\\d+)", "\\1", data_grouped$record)
data_grouped <- data_grouped |> dplyr::select(-model)

cross_validation <- list()
for (i in seq_len(n_folds)) {
  # get the training and validation file names
  train_split_names <- file_names[folds != i]
  test_split_names <- file_names[folds == i]

  train_split <- data_grouped[data_grouped$record %in% train_split_names, ]
  test_split <- data_grouped[data_grouped$record %in% test_split_names, ]


  train_split_stats <- train_split |>
    dplyr::mutate(
      score = score, min = min(score), q25 = quantile(score, 0.25),
      median = quantile(score, 0.5), q75 = quantile(score, 0.75),
      iqr = q75 - q25,
      mean = mean(score), max = max(score),
      sd = sd(score)
    )

  test_split_stats <- test_split |>
    dplyr::mutate(
      score = score, min = min(score), q25 = quantile(score, 0.25),
      median = quantile(score, 0.5), q75 = quantile(score, 0.75),
      iqr = q75 - q25,
      mean = mean(score), max = max(score),
      sd = sd(score)
    )

  cross_validation[[i]] <- list(
    train_split_stats = train_split_stats,
    test_split_stats = test_split_stats
  )

  # plots <- list()
  # for (i in seq_len(length(best_models))) {
  #   dd <- test_split_stats |> dplyr::filter(id_text == best_models[i])
  #   plots[[i]] <- ggplot2::ggplot(dd, ggplot2::aes(x = reorder(record, -score), y = score)) +
  #     ggplot2::geom_point(size = 2) +
  #     ggplot2::geom_hline(ggplot2::aes(yintercept = median), colour = "red") +
  #     ggplot2::geom_hline(ggplot2::aes(yintercept = 0), colour = "gray50") +
  #     ggplot2::scale_y_continuous(
  #       limits = c(0, 1),
  #       expand = c(0.1, 0.05, 0.2, -0.1)
  #     ) +
  #     ggplot2::theme_bw(base_size = 15) +
  #     ggplot2::theme(axis.text.x = ggplot2::element_text(size = 9, angle = 90, vjust = 0.5, hjust = 1)) +
  #     ggplot2::labs(
  #       title = glue::glue("{best_models[i]} - MP window: {dd$window_size[1]}, Regime threshold: {dd$regime_threshold[1]}, Regime landmark: {dd$regime_landmark[1]}"),
  #       x = ifelse(i == length(best_models), "Record ID", ""),
  #       y = ggplot2::element_blank()
  #     )
  # }

  # library(patchwork)

  # wrap_plots(plots, ncol = 1, guides = "collect") + plot_annotation(
  #   title = "Performances of the 3 best models",
  #   theme = ggplot2::theme_bw() + ggplot2::theme(
  #     plot.title = ggplot2::element_text(size = 20)
  #   )
  # )
}

for (i in seq_len(n_folds)) {
  train_split_stats <- cross_validation[[i]]$train_split_stats
  test_split_stats <- cross_validation[[i]]$test_split_stats

  best_models_names <- train_split_stats |>
    dplyr::group_by(id) |>
    dplyr::slice_head() |>
    dplyr::ungroup() |>
    dplyr::arrange(desc(mean), desc(iqr)) |>
    dplyr::slice_tail(n = 3) |>
    dplyr::pull(id_text)

  best_models_train_stats <- train_split_stats |>
    dplyr::filter(id_text %in% best_models_names) |>
    dplyr::slice_head(n = 1) |>
    dplyr::arrange(desc(mean), desc(iqr)) |>
    dplyr::select(
      regime_threshold, window_size, regime_landmark, id_text,
      min, q25, median, q75, iqr, mean, max, sd
    )

  best_models_test_stats <- test_split_stats |>
    dplyr::filter(id_text %in% best_models_names) |>
    dplyr::slice_head(n = 1) |>
    dplyr::arrange(desc(mean), desc(iqr)) |>
    dplyr::select(
      regime_threshold, window_size, regime_landmark, id_text,
      min, q25, median, q75, iqr, mean, max, sd
    )

  cross_validation[[i]]$best_models_train_stats <- best_models_train_stats
  cross_validation[[i]]$best_models_test_stats <- best_models_test_stats
}

cross_validation[[1]]$best_models_train_stats
cross_validation[[1]]$best_models_test_stats
cross_validation[[2]]$best_models_train_stats
cross_validation[[2]]$best_models_test_stats
cross_validation[[3]]$best_models_train_stats
cross_validation[[3]]$best_models_test_stats
cross_validation[[4]]$best_models_train_stats
cross_validation[[4]]$best_models_test_stats
cross_validation[[5]]$best_models_train_stats
cross_validation[[5]]$best_models_test_stats


# save the results
saveRDS(results_df, here("output", glue("{dataname}_cross_validation_results.rds")))
