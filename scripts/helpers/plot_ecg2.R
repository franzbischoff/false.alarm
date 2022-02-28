

plot_ecg2 <- function(data, path = NULL) {
  plots <- list()
  filters <- attr(data, "filters")
  names <- names(filters)
  info <- attr(data, "info")
  for (i in seq_len(length(filters) + 1)) {
    if (i == 1) {
      plots[[i]] <- plotly::plot_ly(
        x = as.POSIXct(seq_len(length(data)), origin = "1970-01-01", tz = "UTC"),
        y = data,
        mode = "lines", type = "scatter", size = I(0.5),
        name = "data"
      )
    } else {
      plots[[i]] <- plotly::plot_ly(
        x = as.POSIXct(seq_len(length(filters[[i - 1]])), origin = "1970-01-01", tz = "UTC"),
        y = filters[[i - 1]],
        mode = "lines", type = "scatter", size = I(1),
        name = names[i - 1]
      )
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
        rangeselector = list(
          visible = TRUE,
          buttons = list(
            list(
              count = 4,
              label = "4 secs",
              step = "second",
              stepmode = "backward"
            ),
            list(
              count = 10,
              label = "10 secs",
              step = "second",
              stepmode = "backward"
            ),
            list(
              count = 30,
              label = "30 secs",
              step = "second",
              stepmode = "backward"
            ),
            list(
              count = 60,
              label = "60 secs",
              step = "second",
              stepmode = "backward"
            ),
            list(step = "all")
          )
        ),
        rangeslider = TRUE
      ),
      yaxis = list(
        autorange = "visible", # not working yet
        fixedrange = FALSE
      )
    )
  }

  fig <- plotly::subplot(plots, nrows = length(names) + 1, shareX = TRUE)

  if (is.null(path)) {
    print(fig)
  } else {
    plotly::export(fig, path)
  }

  return(invisible(fig))
}
