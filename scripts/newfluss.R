
fluss_extract <- function(ecg_data, exclude, num_segments = 3, exclusion_zone = NULL) {
  checkmate::qassert(ecg_data, "L")
  checkmate::qassert(num_segments, "N")
  checkmate::qassert(exclusion_zone, c("0", "N"))

  file <- names(ecg_data)
  checkmate::qassert(file, "S1")
  names <- names(ecg_data[[file]])
  names <- setdiff(names, exclude)

  for (n in names) {
    mp <- attr(ecg_data[[file]][[n]], "mp")

    if (is.null(exclusion_zone)) {
      ez <- mp$ez * 10 # normally ez is 0.5, so ez here is 5
    } else {
      ez <- exclusion_zone
    }

    cac <- mp$cac # keep cac intact
    segments_positions <- vector(mode = "numeric")
    arc_counts_size <- length(cac)
    ez <- round(mp$w * ez + .Machine$double.eps^0.5)

    for (i in 1:num_segments) {
      idx <- which.min(cac)

      if (cac[idx] >= 1) {
        break
      }
      segments_positions[i] <- idx
      cac[max(1, (idx - ez)):min(arc_counts_size, (idx + ez - 1))] <- Inf
    }

    mp$fluss <- segments_positions

    attr(ecg_data[[file]][[n]], "mp") <- mp
  }

  return(ecg_data)
}

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
    readr::write_file(s(), file = here::here("output", sprintf("Fluss_%s_%s.svg", file, n)))
  }
  return(ecg_data)
}

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

compute_arcs <- function(ecg_data, exclude, time_constraint = NULL, exclusion_zone = NULL) {
  checkmate::qassert(ecg_data, "L")
  checkmate::qassert(exclusion_zone, c("0", "N"))
  checkmate::qassert(time_constraint, c("0", "N"))

  file <- names(ecg_data)
  names <- names(ecg_data[[file]])
  names <- setdiff(names, exclude)

  ez <- exclusion_zone

  for (n in names) {
    mp <- attr(ecg_data[[file]][[n]], "mp")

    arc_counts <- vector(mode = "numeric")
    profile_index_size <- length(mp$profile_index)
    nnmark <- vector(mode = "numeric", profile_index_size)

    for (i in 1:profile_index_size) {
      j <- mp$profile_index[i]

      if (j == i) {
        # nnmark[j] <- nnmark[j] + 1
        next
      }

      if (!is.null(time_constraint)) {
        if (abs(j - i) > time_constraint) {
          next
        }
      }

      if (j < 0 || j > profile_index_size) {
        next
      }

      nnmark[min(i, j)] <- nnmark[min(i, j)] + 1
      nnmark[max(i, j)] <- nnmark[max(i, j)] - 1
    }

    arc_counts <- cumsum(nnmark)

    if (is.null(time_constraint)) {
      x <- seq(0, 1, length.out = profile_index_size)

      if (is.null(attr(mp, "subsetting"))) {
        ideal_arc_counts <- stats::dbeta(x, 2, 2) * profile_index_size / 3
      } else {
        ideal_arc_counts <- stats::dbeta(x, 2.1, 2.1) * profile_index_size / 3
      }

      if (is.null(exclusion_zone)) {
        ez <- mp$ez * 10 # normally ez is 0.5, so ez here is 5
      }

      corrected_arc_counts <- pmin(arc_counts / ideal_arc_counts, 1)
      ez <- round(mp$w * ez + .Machine$double.eps^0.5)
      corrected_arc_counts[1:min(ez, profile_index_size)] <- 1.0
      corrected_arc_counts[max((profile_index_size - ez + 1), 1):profile_index_size] <- 1.0
      mp$cac <- corrected_arc_counts
    } else {
      mp$cac <- arc_counts / max(arc_counts)
      mp$cac[1:min(time_constraint, profile_index_size)] <- 1.0
      mp$cac[max((profile_index_size - mp$w * 2 + 1), 1):profile_index_size] <- 1.0
    }

    attr(ecg_data[[file]][[n]], "mp") <- mp
  }

  return(ecg_data)
}
