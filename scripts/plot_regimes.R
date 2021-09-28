plot_regimes <- function(data_with_regimes, params, infos) {
  checkmate::qassert(data_with_regimes, "L2")
  checkmate::qassert(params, "L+")
  checkmate::assert_true(identical(
    attr(data_with_regimes[[1]], "info"),
    attr(data_with_regimes[[2]], "info")
  ))

  data <- data_with_regimes[[1]]
  data_info <- attr(data, "info")
  subset_start <- ifelse(isFALSE(data_info$subset), 0, data_info$subset[1] - 1)
  regimes <- data_with_regimes[[2]]

  pars <- attr(regimes, "params")

  # don't compute if the stats are not compatible
  checkmate::assert_true(
    all(
      pars$window_size == params$window_size,
      pars$time_constraint == params$time_constraint,
      pars$history == params$history
    )
  )

  filename <- infos$filename
  threshold <- params$threshold
  floss_threshold <- params$floss_threshold
  window_size <- params$window_size
  history <- params$history
  time_constraint <- ifelse(params$time_constraint == 0, history, params$time_constraint)
  track <- data_info$label
  alarm <- infos$alarm
  alarm_true <- infos$true
  filter_size <- "raw" # TODO

  file <- sprintf("%s_%s_%d_%.1f_%d_%.1f_%s.png", filename, track, window_size, threshold, time_constraint, floss_threshold, filter_size)
  title <- sprintf("FLOSS-Regimes - %s-%s, w: %d, t: %.1f, ct: %.1f, c: %d, %s-%s", filename, track, window_size, threshold, floss_threshold, time_constraint, alarm, alarm_true)

  a <- plot_ecg_with_regimes(data, regimes, title, params, subset_start, ylim = c(min(data), max(data)))

  ggplot2::ggsave(
    plot = a, filename = here::here("tmp", file),
    device = "png", width = 5, height = 2, scale = 0.8
  )

  return(a)
}

plot_ecg_with_regimes <- function(ecg_data, regimes, title, params, subset_start, ylim) {
  window_size <- params$window_size
  time_constraint <- params$time_constraint
  data_length <- length(ecg_data)
  subset_end <- data_length + subset_start

  data_idxs <- seq.int(subset_start + 1, subset_end)
  y_amp <- (ylim[2] - ylim[1])
  last_3_secs <- subset_end - (3 * 250) # this is the max detection delay needed for Asystole and Vfib
  last_10_secs <- subset_end - (10 * 250) # this is the max detection delay needed for Asystole and Vfib

  aa <- ggplot2::ggplot(data.frame(x = data_idxs, y = ecg_data), ggplot2::aes(x, y)) +
    ggplot2::geom_line(size = 0.1)

  aa <- aa + ggplot2::annotate("segment", y = ylim[1], yend = ylim[1], x = subset_end - window_size, xend = subset_end, color = "blue", size = 0.1) +
    ggplot2::annotate("text", x = subset_end - (window_size / 2), y = ylim[1] + (y_amp * 0.03), label = "Window size", color = "blue", size = 1)

  if (!isFALSE(regimes)) {
    for (trigger in regimes$idxs) {
      aa <- aa + ggplot2::annotate("segment",
        y = ylim[1], yend = ylim[2], x = trigger,
        xend = trigger, color = "orange", size = 0.2
      )
    }

    aa <- aa + ggplot2::annotate("text", x = subset_end - (window_size / 2), y = ylim[1] + (y_amp * 0.95), label = "Change", color = "orange", size = 1.5)
  }

  aa <- aa + ggplot2::annotate("segment", y = ylim[1], yend = ylim[2], x = last_3_secs, xend = last_3_secs, color = "firebrick", size = 0.1) +
    ggplot2::annotate("text", x = last_3_secs - 100, y = ylim[1] + (y_amp * 0.2), label = "Detection limit", color = "firebrick", size = 1.2, angle = 90)

  aa <- aa + ggplot2::annotate("segment", y = ylim[1], yend = ylim[2], x = last_10_secs, xend = last_10_secs, color = "blue", size = 0.1) +
    ggplot2::annotate("text", x = last_10_secs - 100, y = ylim[1] + (y_amp * 0.2), label = "Event limit", color = "blue", size = 1, angle = 90)

  if (time_constraint > 0) {
    curr_ts_constr <- (subset_end - time_constraint)
    aa <- aa + ggplot2::annotate("segment", y = ylim[1], yend = ylim[2], x = curr_ts_constr, xend = curr_ts_constr, color = "red", size = 0.1) +
      ggplot2::annotate("text", x = curr_ts_constr - 100, y = ylim[1] + (y_amp * 0.4), label = "Time constraint", color = "red", size = 1.5, angle = 90)
  }

  aa <- aa + ggplot2::theme_grey(base_size = 7) + ggplot2::theme(
    axis.text.y = ggplot2::element_text(hjust = 0.5, size = 3, angle = 90),
    axis.text.x = ggplot2::element_text(hjust = 0.5, size = 4)
  ) +
    ggplot2::xlim(subset_start, subset_end) +
    ggplot2::ylim(ylim[1], ylim[2]) +
    ggplot2::ggtitle(title) +
    ggplot2::ylab("value") +
    ggplot2::xlab("frames (250hz)") # 2.5 is 250hz/batch_size

  return(aa)
}
