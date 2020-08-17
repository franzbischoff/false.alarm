
plot_ecg <- function(data) {
  plots <- list()
  names <- names(data)
  for (i in 1:ncol(data)) {
    if (i == 1) {
      next
    }

    plots[[i - 1]] <- plot_ly(x = data[, 1], y = data[, i], mode = "lines", type = "scatter", name = names[i])
    plots[[i - 1]] <- layout(plots[[i - 1]],
      title = "Multiplot",
      xaxis = list(
        title = "timeline",
        rangeslider = TRUE
      )
      # yaxis = list(
      #   # autorange = "visible" # not working yet
      #   fixedrange = FALSE
      # )
    )
  }

  fig <- subplot(plots, nrows = ncol(data) - 1, shareX = TRUE)
  print(fig)
}
