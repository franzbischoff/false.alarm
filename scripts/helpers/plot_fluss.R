

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

tkplot <- function(object, interactive = FALSE, res = 50, dataset = c("afib", "magvent", "vtachy")) {
  # fixed configurations
  if (dataset == "afib") {
    basedir <- here::here("inst", "extdata", "afib_regimes")
    rsam_from <- 200
    track <- "II"
  } else if (dataset == "magvent") {
    basedir <- here::here("inst", "extdata", "malignantventricular")
    rsam_from <- 250
    track <- "ECG1"
  } else if (dataset == "vtachy") {
    basedir <- here::here("inst", "extdata", "vtachyarrhythmias")
    rsam_from <- 250
    track <- "ECG"
  }

  ecg <- read_ecg_with_atr(glue::glue(basedir, "/", object$record), resample_from = rsam_from, resample_to = res)
  value <- ecg[[1]][[track]]
  prop <- 250 / res
  mask <- seq.int(50, 100)
  value[1:5] <- median(value[mask])
  value[(length(value) - 5):length(value)] <- median(value[mask])
  time <- seq(1, floor(length(value) * prop), length.out = length(value))
  data <- tibble::tibble(time = time, value = value)
  min_data <- min(data$value)
  max_data <- max(data$value)
  truth <- clean_truth(floor(attr(ecg[[1]], "regimes") * prop), floor(length(value) * prop)) # object$truth[[1]]
  preds <- object$pred[[1]]

  title <- glue::glue(
    "Recording: {object$record} ",
    "#truth: {length(truth)}, ",
    "#preds: {length(preds)}, ",
    "length: {floor(length(value)*prop)} ",
    "FLOSS Score: {round(object$score, 3)}"
  )

  subtitle <- suppressWarnings(glue::glue(
    "Parameters: ",
    "MP window: {object$window_size}, ",
    ifelse(!is.null(object$mp_threshold), "MP threshold: {object$mp_threshold}, ", ""),
    ifelse(!is.null(object$time_constraint), "Time constraint: {object$time_constraint}, ", ""),
    ifelse(!is.null(object$regime_threshold), "Regime threshold: {object$regime_threshold}, ", ""),
    ifelse(!is.null(object$regime_landmark), "Regime landmark: {object$regime_landmark}", "")
  ))

  plot <- data |>
    timetk::plot_time_series(
      time, value,
      .title = glue::glue(title, "<br><sup>{subtitle}</sup>"),
      .interactive = interactive,
      .smooth = FALSE,
      .line_alpha = 0.3,
      .line_size = 0.2,
      .plotly_slider = interactive
    )

  if (interactive) {
    plot <- plot |>
      plotly::add_segments(
        x = preds, xend = preds, y = min_data,
        yend = max_data + (max_data - min_data) * 0.1,
        line = list(width = 2.5, color = "#0108c77f"),
        name = "Predicted"
      ) |>
      plotly::add_segments(
        x = truth, xend = truth, y = min_data,
        yend = max_data,
        line = list(width = 2.5, color = "#ff00007f"),
        name = "Truth"
      )
  } else {
    sci <- getOption("scipen")
    options(scipen = 999)
    plot <- plot +
      ggplot2::geom_segment(
        data = tibble::tibble(pre = preds),
        ggplot2::aes(
          x = pre, xend = pre,
          y = min_data, yend = max_data + (max_data - min_data) * 0.1
        ), linewidth = 1, color = "#0108c77f"
      ) +
      ggplot2::geom_segment(
        data = tibble::tibble(tru = truth),
        ggplot2::aes(
          x = tru, xend = tru,
          y = min_data, yend = max_data - (max_data - min_data) * 0.1
        ), linewidth = 1, color = "#ff00007f"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        legend.position = "none",
        plot.margin = ggplot2::margin(0, 0, 0, 10)
      ) + ggplot2::scale_x_continuous(n.breaks = 6, labels = scales::label_number(scale = 0.004, suffix = "s")) +
      ggplot2::labs(title = title, subtitle = subtitle, y = ggplot2::element_blank())
    options(scipen = sci)
  }
  plot
}
