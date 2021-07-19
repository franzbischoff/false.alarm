plot_arcs <- function(pairs, alpha = NULL, quality = 30, lwd = 15, col = c("blue", "orange"),
                      main = "Arc Plot", ylab = "", xlab = "Profile Index", xmin = NULL, xmax = NULL, ...) {
  if (length(pairs) == 0) {
    warning("No arc to plot.")
    return(NULL)
  }

  if (is.null(xmin)) {
    xmin <- min(pairs)
  }

  if (is.null(xmax)) {
    xmax <- max(pairs)
  }

  max_arc <- max(abs(pairs[, 2] - pairs[, 1]))
  ymax <- (max_arc / 2 + (lwd * lwd) / 8)
  z_seq <- seq(0, base::pi, length.out = quality)
  xlim <- c(xmin, xmax)
  ylim <- c(0, ymax)

  if (is.null(alpha)) {
    alpha <- min(0.5, max(10 / nrow(pairs), 0.03))
  }

  arccolr <- grDevices::adjustcolor(col, alpha.f = alpha)
  if (length(col) > 1) {
    arccoll <- grDevices::adjustcolor(col[2], alpha.f = alpha)
  } else {
    arccoll <- grDevices::adjustcolor(col, alpha.f = alpha)
  }

  # blank plot
  graphics::plot(0.5, 0.5,
    type = "n", main = main, xlab = xlab, ylab = ylab,
    xlim = xlim, ylim = ylim, yaxt = "n", ...
  )

  for (i in seq_len(nrow(pairs))) {
    if (pairs[i, 1] > pairs[i, 2]) {
      arccol <- arccoll
    } else {
      arccol <- arccolr
    }

    x1 <- min(pairs[i, 1], pairs[i, 2])
    x2 <- max(pairs[i, 1], pairs[i, 2])
    center <- (x1 - x2) / 2 + x2
    radius <- (x2 - x1) / 2
    x_seq <- center + radius * cos(z_seq)
    y_seq <- radius * sin(z_seq)
    graphics::lines(x_seq, y_seq,
      col = arccol, lwd = lwd, lty = 1, lend = 1
    )
  }
}
