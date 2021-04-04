#' Plots the ECG data imported using rdmat()
#'
#' @param data The object from rdmat()
#'
#' @return
#' Returns invisibly the generated plot. Also plots using plotly.
#' @export
#'
#' @examples
#' \dontrun{
#' data <- read_ecg("data/a103l")
#' plot_ecg(data)
#' }
#'
plot_ecg <- function(data) {
  checkmate::qassert(data, "L+")
  if (length(data) == 1) {
    data <- data[[1]]
    checkmate::qassert(data, "L>1")
  }

  plots <- list()
  names <- names(data)
  info <- attr(data, "info")
  for (i in seq_len(length(data))) {
    if (i == 1) {
      next
    }

    plots[[i - 1]] <- plotly::plot_ly(
      x = as.POSIXct(data[[1]], origin = "1970-01-01", tz = "UTC"),
      y = data[[i]],
      mode = "lines", type = "scatter",
      name = names[i]
    )
    plots[[i - 1]] <- plotly::layout(plots[[i - 1]],
      margin = list(t = 80, b = 50, l = 50, r = 50, pad = 0),
      title = paste0(
        "Monitor multiplot from file '", info$filename, "'\n",
        info$alarm, " = ", info$true
      ),
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
      )
      # yaxis = list(
      #   autorange = "visible" # not working yet
      #   fixedrange = FALSE
      # )
    )
  }

  fig <- plotly::subplot(plots, nrows = length(data) - 1, shareX = TRUE)
  print(fig)
  return(invisible(fig))
}
