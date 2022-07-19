library(dplyr)
library(here)
library(timetk)
source(here("scripts", "common", "read_ecg.R"), encoding = "UTF-8")
source(here("scripts", "common", "score_floss.R"), encoding = "UTF-8")
source(here("scripts", "common", "get_set_attributes.R"), encoding = "UTF-8")

lst_to_df <- function(lst, keep_attributes = TRUE) {
  new_df <- dplyr::bind_rows(lst)

  if (keep_attributes) {
    nc <- nrow(new_df)
    attributes(new_df) <- attributes(lst[[1]])
    attr(new_df, "row.names") <- seq.int(1, nc)
  }

  new_df$tar_group <- NULL

  return(new_df)
}

# Using targets
# library(targets)
# Sys.setenv(TAR_PROJECT = "regime_optimize")
# tar_load(analysis_fitted)
# all_fits <- lst_to_df(analysis_fitted)

# Using cached data
analysis_fitted <- readRDS(here("output", "regime_outputs.rds"))
all_fitted <- lst_to_df(analysis_fitted$fitted_models)
scores <- all_fitted %>%
  dplyr::select(.predictions) %>%
  tidyr::unnest(.predictions) %>%
  ## mp_threshold of 1 and time_constraint of 750 are unrealistic, so we filter them out
  dplyr::filter(mp_threshold <= 0.9, time_constraint >= 800) %>%
  dplyr::distinct(.id, window_size, time_constraint, mp_threshold, regime_threshold, .keep_all = TRUE) %>%
  dplyr::mutate(truth = clean_truth(truth, .sizes), .pred = clean_pred(.pred)) %>%
  dplyr::group_by(.id, window_size, time_constraint, mp_threshold, regime_threshold) %>%
  dplyr::summarise(
    score = score_regimes(truth[[1]], .pred[[1]], 0),
    pred = .pred
  ) %>%
  dplyr::ungroup() %>%
  dplyr::rename(record = .id)

saveRDS(scores, here::here("analysis", "shiny", "scores.rds"))

# Dataset

files_uri <- here::here("inst", "extdata", "afib_regimes")

dataset <- unique(scores$record)

all_data <- vector(mode = "list")
for (data in dataset) {
  ecg <- read_ecg_with_atr(here::here(files_uri, data), resample_from = 200, resample_to = 50) # downsample for graphics
  value <- ecg[[1]]$II
  value[1:5] <- median(value[50:100])
  value[(length(value) - 5):length(value)] <- median(value[50:100])
  truth <- floor(get_file_regimes(ecg[[1]]) * 5)
  truth <- clean_truth(truth, floor(length(value) * 5))
  time <- seq(1, floor(length(value) * 5), length.out = length(value))
  ecg <- tibble::tibble(time = time, value = value)
  all_data[[data]] <- list(ecg = ecg, truth = truth)
  # ecg %>% plot_time_series(time, value, .smooth = FALSE, .interactive = TRUE)
}

saveRDS(all_data, here::here("analysis", "shiny", "dataset.rds"))

# landmarks


preds_land <- readRDS(here::here("dev", "new_preds_land.rds"))
preds_land$mp_threshold <- 0
preds_land$time_constraint <- 0
saveRDS(preds_land, here::here("analysis", "shiny_land", "scores.rds"))

# Dataset

files_uri <- here::here("inst", "extdata", "afib_regimes")

dataset <- unique(preds_land$record)

all_data <- vector(mode = "list")
for (data in dataset) {
  ecg <- read_ecg_with_atr(here::here(files_uri, data), resample_from = 200, resample_to = 50) # downsample for graphics
  value <- ecg[[1]]$II
  value[1:5] <- median(value[50:100])
  value[(length(value) - 5):length(value)] <- median(value[50:100])
  truth <- floor(get_file_regimes(ecg[[1]]) * 5)
  truth <- clean_truth(truth, floor(length(value) * 5))
  time <- seq(1, floor(length(value) * 5), length.out = length(value))
  ecg <- tibble::tibble(time = time, value = value)
  all_data[[data]] <- list(ecg = ecg, truth = truth)
  # ecg %>% plot_time_series(time, value, .smooth = FALSE, .interactive = TRUE)
}

saveRDS(all_data, here::here("analysis", "shiny_land", "dataset.rds"))


a <- readRDS(here::here("analysis", "shiny", "scores.rds"))
a <- a %>%
  # group_by(window_size, regime_threshold, regime_landmark) %>%
  group_by(window_size, time_constraint, mp_threshold, regime_threshold) %>%
  summarise(mean = mean(score), min = min(score), max = max(score), med = median(score), q25 = quantile(score, .25), q75 = quantile(score, .75), mm_diff = mean(score) - median(score)) %>%
  ungroup()
a
# a <- a %>% mutate(mm_diff = abs(mean - med) < 2)

library(plotly)
conflict_prefer("layout", "plotly")
axis <- list(
  showline = FALSE,
  zeroline = FALSE,
  gridcolor = "#ffff",
  ticklen = 4
)

pl_colorscale <- list(
  c(0, "rgba(0,230,0,0.5)"),
  c(0.2, "rgba(230,0,0,0.05)"),
  c(0.2001, "rgba(0,0,230,0.05)"),
  c(1, "rgba(0,0,230,0.05)")
)

fig <- a %>%
  filter(mm_diff <= 1.0) %>%
  plot_ly()
fig <- fig %>%
  add_trace(
    type = "splom",
    dimensions = list(
      list(label = "window_size", values = ~window_size),
      list(label = "regime_threshold", values = ~regime_threshold),
      list(label = "time_constraint", values = ~time_constraint),
      list(label = "mp_threshold", values = ~mp_threshold),
      # list(label = "regime_landmark", values = ~regime_landmark),
      list(label = "mean", values = ~mean),
      list(label = "med", values = ~med)
    ),
    marker = list(
      size = 9,
      color = ~mean,
      autocolorscale = FALSE,
      colorscale = pl_colorscale,
      line = list(
        width = 0.5,
        color = "rgba(230,230,230, 0.1)"
      )
    ),
    selected = list(
      marker = list(
        color = "rgb(0, 230, 0)",
        opacity = 0.7,
        size = 5
      )
    )
  )
fig <- fig %>%
  layout(
    title = "FLOSS",
    hovermode = "closest",
    dragmode = "select",
    plot_bgcolor = "rgba(240,240,240, 0.8)",
    xaxis = list(domain = NULL, showline = FALSE, zeroline = FALSE, gridcolor = "#ffff", ticklen = 4),
    yaxis = list(domain = NULL, showline = FALSE, zeroline = FALSE, gridcolor = "#ffff", ticklen = 4),
    xaxis2 = axis,
    xaxis3 = axis,
    xaxis4 = axis,
    yaxis2 = axis,
    yaxis3 = axis,
    yaxis4 = axis
  ) %>%
  style(diagonal = list(visible = FALSE), showupperhalf = FALSE)

fig
