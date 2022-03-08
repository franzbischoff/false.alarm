

plot_ecg2 <- function(data, path = NULL, width = 1300, height = 1000) {
  plots <- list()
  filters <- attr(data, "filters")
  names <- names(filters)
  info <- attr(data, "info")
  for (i in seq_len(length(filters) + 1)) {
    ymin <- 0
    ymax <- 1000
    if (i == 1) {
      plots[[i]] <- plotly::plot_ly(
        x = as.POSIXct(seq_len(length(data)), origin = "1970-01-01", tz = "UTC"),
        y = data,
        mode = "lines", type = "scatter", size = I(0.5),
        name = "data"
      )
      ymin <- min(data)
      ymax <- max(data)
    } else {
      plots[[i]] <- plotly::plot_ly(
        x = as.POSIXct(seq_len(length(filters[[i - 1]])), origin = "1970-01-01", tz = "UTC"),
        y = filters[[i - 1]],
        mode = "lines", type = "scatter", size = I(1),
        name = names[i - 1]
      )
      ymin <- 0
      ymax <- max(filters[[i - 1]][1000:2000]) * 1.2
    }
    plots[[i]] <- plotly::layout(plots[[i]],
      margin = list(t = 80, b = 50, l = 50, r = 50, pad = 0),
      title = paste0(
        "Monitor multiplot track '", info$signal, "'\n",
        "gain = ", info$gain, " ", info$unit
      ),
      # title = paste0(
      #   "Monitor multiplot file '", info$filename, "'\n",
      #   info$alarm, " = ", info$true
      # ),
      xaxis = list(
        title = "time",
        type = "date",
        ticklen = 5,
        tickformat = "%M:%S.%2f",
        # rangeselector = list(
        #   visible = TRUE,
        #   buttons = list(
        #     list(
        #       count = 4,
        #       label = "4 secs",
        #       step = "second",
        #       stepmode = "backward"
        #     ),
        #     list(
        #       count = 10,
        #       label = "10 secs",
        #       step = "second",
        #       stepmode = "backward"
        #     ),
        #     list(
        #       count = 30,
        #       label = "30 secs",
        #       step = "second",
        #       stepmode = "backward"
        #     ),
        #     list(
        #       count = 60,
        #       label = "60 secs",
        #       step = "second",
        #       stepmode = "backward"
        #     ),
        #     list(step = "all")
        #   )
        # ),
        rangeslider = TRUE
      ),
      yaxis = list(
        range = list(ymin, ymax),
        fixedrange = FALSE
      )
    )
  }

  fig <- plotly::subplot(plots, nrows = length(names) + 1, shareX = TRUE)

  if (is.null(path)) {
    print(fig)
  } else {
    # plotly::export(fig, path)
    reticulate::use_miniconda("r-reticulate")
    plotly::save_image(fig, path, width = width, height = height)
  }

  return(invisible(fig))
}


# i <- 2
# s <- 2000:25000
# plot.ts(cbind(
#   filters[[i]]$II$complex[s],
#   filters[[i]]$II$complex2[s],
#   filters[[i]]$II$complex3[s],
#   filters[[i]]$II$complex4[s],
#   filters[[i]]$II$complex5[s],
#   filters[[i]]$II$complex6[s]
# ), nc = 1)

# for (i in 1:15) {
#   data <- dataset[[i]]$II
#   attr(data, "filters") <- filters[[i]]$II
#   # plot_ecg2(data)
#   fle <- paste0("teste3-", i, ".pdf")
#   plot_ecg2(data, here::here("tmp", fle), 1323, 992)
#   print(paste("Num", i))
# }

library(tibble)
source("/workspace/false.alarm/scripts/common/get_set_attributes.R", encoding = "UTF-8")
tar_load(c("dataset", "filters", "filters2"))

s1 <- 2001:3000
s2 <- 2001:30000
# # skew > -0.5 & < 1.8

neg <- c(4, 7, 8, 10, 11, 12) # , 13) # , 14) # 14 is noisy 5-8k and 13.7-16k
pos <- c(1, 2, 3, 5, 6, 9) # , 15) # 15 noise between 7.5k and 9k

# Gain was not applied to the data
result_thr <- NULL
# First set the normal threshold using datasets we know there is no artifacts
for (i in neg) {
  gg <- get_info(filters[[i]]$II)$gain
  res <- purrr::map_dfr(filters[[i]]$II, function(x, gain = gg) {
    x2 <- x / gain * 5000
    xs <- x[s1]
    x2s <- x2[s1]
    # select on the start of the dataset, the points that are building the baseline
    # for that, we exclude the points below the mean and retrieve the quantile 0.8 from
    # the remaining points. The threshold will be for example 1.5 times this value.
    xs_mean <- xs[xs > mean(xs, na.rm = TRUE)]
    x2s_mean <- x2s[x2s > mean(x2s, na.rm = TRUE)]
    xs_thr <- quantile(xs_mean, 0.80, na.rm = TRUE)
    x2s_thr <- quantile(x2s_mean, 0.80, na.rm = TRUE)

    tibble(
      id = i,
      thr = xs_thr,
      thr2 = x2s_thr,
    )
  }, .id = "filter")
  result_thr <- rbind(result_thr, res)
}

# here we will store the throlds
result_thr <- result_thr %>%
  dplyr::group_by(filter) %>%
  dplyr::summarize(thr = max(thr, na.rm = TRUE), thr2 = max(thr2, na.rm = TRUE))


result_all <- NULL
# this vector contains multipliers for each throld if needed
mult <- c(
  cplx_n = 1.7, cplx_r = 1.7,
  act_n = 1.7, act_r = 1.7,
  ampl_n = 1.7, ampl_r = 1.7,
  1.7, 1.7,
  1.7, 1.7
)

# now, let's see if there are any false negative
for (i in neg) {
  gg <- get_info(filters[[i]]$II)$gain
  for (j in seq.int(length(filters[[i]]$II))) {
    filt <- names(filters[[i]]$II[j])
    thr <- dplyr::filter(result_thr, filter == filt)$thr * mult[j]
    thr2 <- dplyr::filter(result_thr, filter == filt)$thr2 * mult[j]
    x <- filters[[i]]$II[[j]]
    x2 <- x / gg * 5000
    xall <- x[s2] / thr
    xall <- xall[!is.na(xall)]
    xall <- xall[xall >= 1]
    x2all <- x2[s2] / thr2
    x2all <- x2all[!is.na(x2all)]
    x2all <- x2all[x2all >= 1]

    result_all <- rbind(
      result_all,
      tibble(
        filter = paste0(filt, "_N"),
        type = "N",
        id = i,
        thr = thr,
        thr2 = thr2,
        all_data = median(xall, na.rm = TRUE),
        all_data_gain = median(x2all, na.rm = TRUE)
      )
    )
  }
}

# and false positives
for (i in pos) {
  gg <- get_info(filters[[i]]$II)$gain
  for (j in seq.int(length(filters[[i]]$II))) {
    filt <- names(filters[[i]]$II[j])
    thr <- dplyr::filter(result_thr, filter == filt)$thr * mult[j]
    thr2 <- dplyr::filter(result_thr, filter == filt)$thr2 * mult[j]
    x <- filters[[i]]$II[[j]]
    x2 <- x / gg * 5000
    xall <- x[s2] / thr
    xall <- xall[!is.na(xall)]
    xall <- xall[xall >= 1]
    x2all <- x2[s2] / thr2
    x2all <- x2all[!is.na(x2all)]
    x2all <- x2all[x2all >= 1]

    result_all <- rbind(
      result_all,
      tibble(
        filter = paste0(filt, "_P"),
        type = "P",
        id = i,
        thr = thr,
        thr2 = thr2,
        all_data = median(xall, na.rm = TRUE),
        all_data_gain = median(x2all, na.rm = TRUE)
      )
    )
  }
}

library(dplyr)
res <- tidyr::pivot_longer(result_all, cols = 4:last_col(), names_to = "metric")
res <- dplyr::filter(res, metric %in% c("all_data", "all_data_gain"))

library(plotly)
fig <- plot_ly(res, type = "box", y = ~value, x = ~filter, color = ~metric)
fig <- fig %>% plotly::layout(boxmode = "group")
fig

# False Positives
result_neg <- result_all %>% dplyr::filter(type == "N")
result_neg %>%
  dplyr::group_by(filter) %>%
  dplyr::summarise(normal = sum(is.na(all_data)) - 7, gain = sum(is.na(all_data_gain)) - 7)

# False negatives
result_pos <- result_all %>% dplyr::filter(type == "P")
result_pos %>%
  dplyr::group_by(filter) %>%
  dplyr::summarise(normal = sum(!is.na(all_data)) - 6, gain = sum(!is.na(all_data_gain)) - 6)


# for (i in c(pos, neg)) {
#   gg <- get_info(filters[[i]]$II)$gain
#   res <- purrr::map_dfr(filters[[i]]$II, function(x, gain = gg) {
#     # x <- x[2000:8000]
#     x2 <- x / gain * 5000
#     qi <- which(x > quantile(x[s1], 0.95, na.rm = TRUE))
#     qgi <- which(x2 > quantile(x2[s1], 0.95, na.rm = TRUE))
#     tibble(
#       id = i,
#       maxi = quantile(x[s1], 0.95, na.rm = TRUE),
#       maxt = median(x[qi], na.rm = TRUE),
#       mu = mean(x[s2], na.rm = TRUE),
#       md = median(x[s2], na.rm = TRUE),
#       sd = sd(x[s2], na.rm = TRUE),
#       g_maxi = quantile(x2[s1], 0.95, na.rm = TRUE),
#       g_maxt = median(x2[qgi], na.rm = TRUE),
#       g_mu = mean(x2[s2], na.rm = TRUE),
#       g_md = median(x2[s2], na.rm = TRUE),
#       g_sd = sd(x2[s2], na.rm = TRUE)
#     )
#   }, .id = "filter")
#   result <- rbind(result, res)
# }
# library(dplyr)
# res <- tidyr::pivot_longer(result, cols = 3:last_col(), names_to = "metric")
# # res <- res %>% mutate_at(vars(value), ~ case_when(filter == "complex" ~ . * 100, TRUE ~ .))

# library(plotly)

# fig <- plot_ly(res, type = "box", y = ~value, x = ~filter, color = ~metric)
# fig <- fig %>% plotly::layout(boxmode = "group")
# fig

# tar_load(c("dataset", "filters"))

# for (i in 1:15) {
#   data <- dataset[[i]]$II
#   attr(data, "filters") <- filters[[i]]$II
#   fle <- paste0("teste4-", i, ".pdf")
#   plot_ecg2(data, here::here("tmp", fle))
# }
