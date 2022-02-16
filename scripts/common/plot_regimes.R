plot_regimes <- function(data_with_regimes, params, infos, save = FALSE) {
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
      pars$mp_time_constraint == params$mp_time_constraint,
      pars$history == params$history
    )
  )

  filename <- infos$filename
  threshold <- params$threshold
  regime_threshold <- params$regime_threshold
  window_size <- params$window_size
  history <- params$history
  mp_time_constraint <- ifelse(params$mp_time_constraint == 0, history, params$mp_time_constraint)
  floss_time_constraint <- ifelse(params$floss_time_constraint == 0, history, params$floss_time_constraint)
  signal <- data_info$signal
  alarm <- infos$alarm
  alarm_true <- infos$true
  filter_size <- "raw" # HACK

  file <- sprintf(
    "%s_%s_%.1f_%d_%d_%.1f_%d_%s.png", filename, signal, regime_threshold, floss_time_constraint, mp_time_constraint, threshold, window_size, filter_size
  )
  title <- sprintf(
    "FLOSS-Regimes - %s-%s, w: %d, t: %.1f, ct: %.1f, c: %d, fc: %d, %s-%s", filename, signal, window_size, threshold,
    regime_threshold, mp_time_constraint, floss_time_constraint, alarm, alarm_true
  )

  a <- plot_ecg_with_regimes(data, regimes, title, params, subset_start, ylim = c(min(data), max(data)))

  if (save || params$save_png) {
    ggplot2::ggsave(
      plot = a, filename = here::here("tmp", file),
      device = "png", width = 5, height = 2, scale = 0.8
    )
  }

  return(a)
}

plot_ecg_with_regimes <- function(ecg_data, regimes, title, params, subset_start, ylim) {
  window_size <- params$window_size
  mp_time_constraint <- params$mp_time_constraint
  data_length <- length(ecg_data)
  subset_end <- data_length + subset_start

  xlim <- c(subset_start + 1, subset_end)
  x_amp <- subset_end - (subset_start + 1)

  data_idxs <- seq.int(subset_start + 1, subset_end)
  y_amp <- (ylim[2] - ylim[1])
  last_3_secs <- subset_end - (3 * 250) # this is the max detection delay needed for Asystole and Fibv
  last_10_secs <- subset_end - (10 * 250) # this is the max detection delay needed for Asystole and Fibv

  aa <- ggplot2::ggplot(data.frame(x = data_idxs, y = ecg_data), ggplot2::aes(x, y)) +
    ggplot2::geom_line(size = 0.1)

  aa <- aa + ggplot2::annotate("segment", y = ylim[1], yend = ylim[1], x = subset_end - window_size, xend = subset_end, color = "blue", size = 0.1) +
    ggplot2::annotate("text", x = subset_end - window_size / 2, y = ylim[1] + y_amp * 0.01, label = "Window size", color = "blue", size = 1, vjust = 0, hjust = 0.5)

  if (!isFALSE(regimes)) {
    for (trigger in regimes$idxs) {
      aa <- aa + ggplot2::annotate("segment",
        y = ylim[1], yend = ylim[2], x = trigger,
        xend = trigger, color = "orange", size = 0.2
      )
    }

    aa <- aa + ggplot2::annotate("text", x = xlim[2], y = ylim[2], label = "Change", color = "orange", size = 1.2, vjust = 0, hjust = 0)
  }

  aa <- aa + ggplot2::annotate("segment", y = ylim[1], yend = ylim[2], x = last_3_secs, xend = last_3_secs, color = "firebrick", size = 0.1) +
    ggplot2::annotate("text", x = last_3_secs - 30, y = ylim[1], label = "Detection limit", color = "firebrick", size = 1, angle = 90, vjust = 0, hjust = 0)

  aa <- aa + ggplot2::annotate("segment", y = ylim[1], yend = ylim[2], x = last_10_secs, xend = last_10_secs, color = "blue", size = 0.3) +
    ggplot2::annotate("text", x = last_10_secs - 30, y = ylim[2], label = "Event limit", color = "blue", size = 1, angle = 90, vjust = 0, hjust = 1)

  if (mp_time_constraint > 0) {
    curr_ts_constr <- (subset_end - mp_time_constraint)
    aa <- aa + ggplot2::annotate("segment", y = ylim[1], yend = ylim[2], x = curr_ts_constr, xend = curr_ts_constr, color = "red", size = 0.1) +
      ggplot2::annotate("text", x = curr_ts_constr - 30, y = ylim[1], label = "Time constraint", color = "red", size = 1, angle = 90, vjust = 0, hjust = 0)
  }

  aa <- aa + ggplot2::theme_grey(base_size = 6) + ggplot2::theme(
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
