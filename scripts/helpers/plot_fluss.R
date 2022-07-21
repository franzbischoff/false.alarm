

plot_fluss <- function(ecg_data, type = c("data", "matrix"),
                       main = "Fast Low-cost Unipotent Semantic Segmentation", xlab = "index",
                       ylab = "", ...) {
  checkmate::qassert(ecg_data, "L")
  checkmate::qassert(type, "S")

  # not needed when using svglite string
  # def_par <- graphics::par(no.readonly = TRUE)
  # on.exit(graphics::par(def_par))

  file <- names(ecg_data)
  checkmate::qassert(file, "S1")
  names <- names(ecg_data[[file]])
  names <- setdiff(names, c("time", "ABP", "PLETH", "RESP"))

  for (n in names) {
    "!DEBUG Creating Fluss Plot"

    mp <- attr(ecg_data[[file]][[n]], "mp")

    type <- match.arg(type)

    if (type == "data") {
      plot_data <- ecg_data[[file]][[n]]
      data_lab <- ylab
      data_main <- "Data"
    } else {
      plot_data <- c(mp$matrix_profile, rep(NA, min(mp$w) - 1))
      data_lab <- "distance"
      data_main <- "Matrix Profile"
    }

    info <- attr(ecg_data[[file]], "info")
    filters <- attr(ecg_data[[file]][[n]], "filter")

    alarm_color <- ifelse(info$true, "orange", "blue")

    fluss_idx <- sort(mp$fluss)

    fluss_size <- length(fluss_idx) + 1
    pairs <- matrix(0, fluss_size, 2)

    offset <- attr(mp, "offset")
    offset <- ifelse(is.null(offset), 0, offset)

    for (i in seq_len(fluss_size)) {
      if (i == 1) {
        pairs[i, 1] <- offset
      } else {
        pairs[i, 1] <- fluss_idx[i - 1] + offset
      }

      if (i == fluss_size) {
        pairs[i, 2] <- length(mp$matrix_profile) + offset
      } else {
        pairs[i, 2] <- fluss_idx[i] + offset
      }
    }

    xnum <- seq_len(length(mp$matrix_profile) + min(mp$w) - 1) + offset
    xmin <- min(xnum)
    xmax <- max(xnum)
    xlim <- c(xmin, xmax)

    s <- svglite::svgstring(16, 10)
    graphics::layout(matrix(c(1, 2, 3), ncol = 1, byrow = TRUE))
    graphics::par(oma = c(1, 1, 3, 0), cex.lab = 1.5)
    plot_arcs(pairs, xlab = xlab, xmin = xmin, xmax = xmax, col = c(alarm_color, alarm_color), ...)
    graphics::mtext(text = main, font = 2, cex = 1.5, outer = TRUE)
    graphics::plot(xnum, plot_data, main = data_main, type = "l", lwd = 0.2, xlab = xlab, ylab = data_lab, xlim = xlim, ...)
    graphics::abline(v = c(72500, 75000), col = c("green", "red"), lwd = 1)
    graphics::points(cbind(which(filters$complex_lim), 0), col = "blue")
    graphics::plot(xnum, c(mp$cac, rep(NA, min(mp$w) - 1)),
      main = "Arc count", type = "l", xlab = xlab, ylab = "normalized count", xlim = xlim, ylim = c(0, 1), ...
    )
    dev.off()
    attr(ecg_data[[file]][[n]], "plot") <- s()

    # good idea to have the files while debugging
    readr::write_file(s(), file = here::here("output", glue::glue("Fluss_{file}_{n}.svg")))
  }
  return(ecg_data)
}
