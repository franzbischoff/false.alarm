

plot_filters <- function(ecg_data) {
  checkmate::qassert(ecg_data, "L")

  file <- names(ecg_data)
  checkmate::qassert(file, "S1")
  names <- names(ecg_data[[file]])
  names <- setdiff(names, c("time", "PLETH", "ABP"))

  def_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(def_par))

  graphics::par(mfrow = c(1, 1))

  split <- (60000:74801)

  for (n in names) {
    filters <- attr(ecg_data[[file]][[n]], "filters")
    plot(scale(ecg_data[[file]][[n]][split], scale = FALSE),
      type = "l",
      main = paste(file, n),
      ylim = c(-3, 20)
    )
    lines(filters$complex[split], col = "red")
    lines(abs(filters$kurtosis[split]), col = "green")
    lines(abs(filters$skewness[split]), col = "blue")
    # points(filters$wander, col = "red", pch = "x")
    # points(filters$mean_more, col = "blue", pch = "+")
    abline(v = 72500 - split[1], col = "black")
  }
}
