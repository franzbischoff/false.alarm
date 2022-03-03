

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

for (i in 1:15) {
  data <- dataset[[i]]$II
  attr(data, "filters") <- filters[[i]]$II
  # plot_ecg2(data)
  fle <- paste0("teste3-", i, ".pdf")
  plot_ecg2(data, here::here("tmp", fle), 1323, 992)
  print(paste("Num", i))
}

tar_load(c("dataset", "filters", "filters2"))
result <- NULL
s1 <- 2000:3000
s2 <- 2000:30000
# skew > -0.5 & < 1.8

neg <- c(4, 7, 8, 10, 11, 12, 13) #, 14) # 14 is noisy 5-8k and 13.7-16k
pos <- c(1, 2, 3, 5, 6, 9) #, 15) # 15 noise between 7.5k and 9k
for (i in c(pos, neg)) {
  gg <- get_info(filters[[i]]$II)$gain
  res <- purrr::map_dfr(filters[[i]]$II, function(x, gain = gg) {
    # x <- x[2000:8000]
    x2 <- x / gain * 5000
    qi <- which(x > quantile(x[s1], 0.95, na.rm = TRUE))
    qgi <- which(x2 > quantile(x2[s1], 0.95, na.rm = TRUE))
    tibble(
      id = i,
      maxi = quantile(x[s1], 0.95, na.rm = TRUE),
      maxt = median(x[qi], na.rm = TRUE),
      mu = mean(x[s2], na.rm = TRUE),
      md = median(x[s2], na.rm = TRUE),
      sd = sd(x[s2], na.rm = TRUE),
      g_maxi = quantile(x2[s1], 0.95, na.rm = TRUE),
      g_maxt = median(x2[qgi], na.rm = TRUE),
      g_mu = mean(x2[s2], na.rm = TRUE),
      g_md = median(x2[s2], na.rm = TRUE),
      g_sd = sd(x2[s2], na.rm = TRUE)
    )
  }, .id = "filter")
  result <- rbind(result, res)
}
library(dplyr)
res <- tidyr::pivot_longer(result, cols = 3:last_col(), names_to = "metric")
# res <- res %>% mutate_at(vars(value), ~ case_when(filter == "complex" ~ . * 100, TRUE ~ .))

library(plotly)

fig <- plot_ly(res, type = "box", y = ~value, x = ~filter, color = ~metric)
fig <- fig %>% plotly::layout(boxmode = "group")
fig

# for(i in 1:15) {
#   data2 <- dataset[[i]]$II
#   attr(data2, "filters") <- filters2[[i]]$II
#   fle <- paste0("teste2-", i, ".pdf")
#   plot_ecg2(data2, here::here("tmp", fle))
# }
