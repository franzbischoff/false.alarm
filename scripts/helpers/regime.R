tar_load(dataset)
data <- dataset[[2]]
rm(dataset)

tar_load(regimes_0.3_2500_0_0_250)
regime <- regimes_0.3_2500_0_0_250[[2]]
rm(regimes_0.3_2500_0_0_250)

idxs <- regime$II$idxs

ann <- get_file_annotations(data)
reg <- get_file_regimes(data)

options(vsc.dev.args = list(width = 1000 * 5, height = 500))

plotly::layout(
  plotly::plot_ly(
    y = data$II, mode = "lines",
    type = "scatter",
    line = list(width = 0.5, color = "#1f77b4"),
    name = "Data"
  ) %>%
    plotly::add_segments(
      x = reg, xend = reg, y = min(data$II),
      yend = max(data$II),
      line = list(width = 1.5, color = "#ff00007f"),
      name = "True"
    ) %>%
    plotly::add_segments(
      x = idxs, xend = idxs, y = min(data$II) * 0.85,
      yend = max(data$II) * 1.15,
      line = list(width = 1.5, color = "#01c7017f"),
      name = "Predicted"
    ) %>%
    plotly::config(
      displaylogo = FALSE, scrollZoom = TRUE,
      toImageButtonOptions = list(format = "svg")
    ),
  margin = list(t = 80, b = 50, l = 50, r = 50, pad = 0),
  title = glue::glue("Regime change detection"),
  xaxis = list(
    title = "time",
    type = "linear",
    ticklen = 5,
    ticks = "inside",
    rangeslider = TRUE
  )
)

plot(data$II, type = "l")
abline(v = reg, col = "green")
abline(v = idxs, col = "red")
