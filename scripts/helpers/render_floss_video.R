render_floss_video <- function(video_file = here::here("dev", "floss_default.mp4"), ecg_data,
                               arc_counts, title = "Online Semantic Segmentation", framerate = 25,
                               filter_w_size = 0, tail_clip = 0, temp_dir = "tmp", subset = FALSE, sample = FALSE) {
  done <- FALSE
  checkmate::qassert(video_file, "S")
  checkmate::qassert(ecg_data, "N+")
  checkmate::qassert(arc_counts, "L")

  # Register exit point ----------------------
  "!DEBUG Register exit point"
  on.exit({
    # try(parallel::stopCluster(cl))
    if (done) {
      if (isTRUE(sample)) {
        return(gg)
      } else {
        return(invisible(done))
      }
    } else {
      rlang::warn("Something went wrong. Anything produced is at ", here::here("tmp", temp_dir), "/")
      return(invisible(FALSE))
    }
  })

  rate <- 250
  last_ten_secs <- 290 * rate # 10 seconds from the end

  params <- attr(arc_counts, "params")

  window_size <- params$window_size
  data_constraint <- params$history
  mp_time_constraint <- params$mp_time_constraint ##
  floss_time_constraint <- params$floss_time_constraint
  landmark <- data_constraint - params$regime_landmark
  batch_size <- arc_counts[[2]]$offset - arc_counts[[1]]$offset

  abs_event_line <- last_ten_secs
  info <- attr(ecg_data, "info")
  abs_subset_start <- ifelse(isFALSE(info$subset), 0, info$subset[1] - 1)

  if (!is.null(info$filter)) {
    filter <- head(info$filter, abs_event_line - filter_w_size + 1)
    event_offset <- sum(filter) # total of points skipped by the filter
    abs_event_line <- abs_event_line - event_offset
  }

  if (dir.exists(here::here("tmp", temp_dir))) {
    unlink(here::here("tmp", temp_dir))
  }

  dir.create(here::here("tmp", temp_dir), recursive = TRUE, showWarnings = FALSE)

  rlang::inform("This will take a while:")
  cat("Plotting frames.")


  ## *** Parallel messes up with min and max... dont use
  ## cl <- parallel::makePSOCKcluster(4, outfile = "")
  ## doParallel::registerDoParallel(cl)
  ## `%dopar%` <- foreach::`%dopar%`
  ## foreach::foreach(d = seq_along(arc_counts)) %dopar% {

  curr_cac_min <- NULL
  data_min <- min(ecg_data)
  data_max <- max(ecg_data)

  for (d in seq_along(arc_counts)) {
    if (!isFALSE(subset)) {
      min_s <- min(subset)
      max_s <- max(subset)

      if (!(arc_counts[[d]]$offset <= max_s && arc_counts[[d]]$offset >= min_s)) {
        next
      }
    }

    if (isTRUE(sample)) {
      d <- length(arc_counts) - 5
    }

    data_idxs_subset <- seq.int((arc_counts[[d]]$offset - abs_subset_start) - data_constraint + 1, (arc_counts[[d]]$offset - abs_subset_start))

    aa <- plot_ecg_streaming(ecg_data[data_idxs_subset], data_constraint, window_size,
      mp_time_constraint = mp_time_constraint,
      floss_time_constraint = floss_time_constraint,
      offset = arc_counts[[d]]$offset,
      trigger_abs_idx = arc_counts[[d]]$trigger$trigger_abs_idx,
      ylim = c(data_min, data_max),
      batch_size = batch_size, rate = rate, abs_event_line = abs_event_line
    )

    # cac_min_idx <- which.min(arc_counts[[d]]$cac[seq.int(kumaraswamy_idx, cac_size)]) + kumaraswamy_idx - 1

    bb <- plot_cac_streaming(arc_counts[[d]]$cac, data_constraint, window_size, mp_time_constraint, floss_time_constraint,
      arc_counts[[d]]$offset,
      tail_clip = tail_clip, batch_size = batch_size, rate = rate, curr_cac_min
    )
    curr_cac_min <- bb$curr_cac_min

    cc <- plot_raw_arcs(arc_counts[[d]]$arcs, arc_counts[[d]]$iac, data_constraint, mp_time_constraint, floss_time_constraint,
      arc_counts[[d]]$offset, batch_size,
      rate = rate
    )

    gg <- gridExtra::grid.arrange(aa, bb, cc,
      nrow = 3, newpage = FALSE,
      top = grid::textGrob(title,
        gp = grid::gpar(fontsize = 8)
      )
    )

    if (isTRUE(sample)) {
      ggplot2::ggsave(
        plot = gg, filename = here::here("dev", glue::glue("{fs::path_file(video_file)}_{d}.png")),
        device = "png", width = 5, height = 7, scale = 0.8
      )

      done <- TRUE
      return(gg)
    }


    ggplot2::ggsave(
      plot = gg, filename = here::here("tmp", temp_dir, glue_fmt("plot{d:03d}.png")),
      device = "png", width = 5, height = 7, scale = 0.8
    )
    if (!(d %% 10)) {
      cat(".")
    }
  }

  cat("\n")

  rlang::inform("Starting Magick.")

  library(magick)
  ## list file names and read in
  imgs <- list.files(here::here("tmp", temp_dir), pattern = "*.png", full.names = TRUE)

  rlang::inform("This will take a while:")

  rlang::inform("- Loading plots.")
  img_list <- lapply(imgs, magick::image_read)

  rlang::inform("- Merging frames.")
  ## join the images together
  img_joined <- magick::image_join(img_list)

  rlang::inform("- Encoding the video.")
  magick::image_write_video(img_joined, path = video_file, framerate = framerate)

  rlang::inform("- Removing temporary files.")
  file.remove(imgs)

  done <- TRUE
  rlang::inform("Done.")
}

plot_ecg_streaming <- function(ecg_data, data_constraint, window_size, mp_time_constraint, floss_time_constraint,
                               offset = 0, trigger_abs_idx = 0, ylim = c(min(ecg_data), max(ecg_data)),
                               batch_size = 100, rate = 250, abs_event_line = 290 * rate) {
  data_idxs <- seq.int(1, data_constraint)
  y_amp <- (ylim[2] - ylim[1])
  last_3_secs <- length(ecg_data) - (3 * rate) # this is the max detection delay needed for Asystole and Fibv

  aa <- ggplot2::ggplot(data.frame(x = data_idxs, y = ecg_data), ggplot2::aes(x, y)) +
    ggplot2::geom_line(size = 0.1)

  aa <- aa + ggplot2::annotate("segment",
    y = ylim[1], yend = ylim[1], x = data_constraint - window_size,
    xend = data_constraint, color = "blue", size = 0.1
  ) +
    ggplot2::annotate("text", x = data_constraint - (window_size / 2), y = ylim[1] + (y_amp * 0.03), label = "Window size", color = "blue", size = 1)

  if (!is.null(trigger_abs_idx) && trigger_abs_idx > 0) {
    curr_trigger_line <- data_constraint - (offset - trigger_abs_idx)
    aa <- aa + ggplot2::annotate("segment",
      y = ylim[1], yend = ylim[2], x = curr_trigger_line,
      xend = curr_trigger_line, color = "orange", size = 0.2
    ) +
      ggplot2::annotate("text", x = data_constraint - (window_size / 2), y = ylim[1] + (y_amp * 0.95), label = "Change", color = "orange", size = 1.5)
  }

  aa <- aa + ggplot2::annotate("segment", y = ylim[1], yend = ylim[2], x = last_3_secs, xend = last_3_secs, color = "firebrick", size = 0.1) +
    ggplot2::annotate("text",
      x = last_3_secs - 40, y = ylim[1] + (y_amp * 0.13), label = "Detection limit",
      color = "firebrick", size = 1.2, angle = 90
    )

  if (offset > abs_event_line) {
    curr_event_line <- data_constraint - (offset - abs_event_line)
    aa <- aa + ggplot2::annotate("segment", y = ylim[1], yend = ylim[2], x = curr_event_line, xend = curr_event_line, color = "blue", size = 0.1) +
      ggplot2::annotate("text", x = curr_event_line - 40, y = ylim[1] + (y_amp * 0.2), label = "Event limit", color = "blue", size = 1, angle = 90)
  }

  if (mp_time_constraint > 0) {
    curr_ts_constr <- (data_constraint - mp_time_constraint)
    aa <- aa + ggplot2::annotate("segment", y = ylim[1], yend = ylim[2], x = curr_ts_constr, xend = curr_ts_constr, color = "red", size = 0.1) +
      ggplot2::annotate("text",
        x = curr_ts_constr - 40, y = ylim[1] + (y_amp * 0.4), label = "Time constraint",
        color = "red", size = 1.5, angle = 90
      )
  }

  aa <- aa + ggplot2::theme_grey(base_size = 7) + ggplot2::theme(
    axis.text.y = ggplot2::element_text(hjust = 0.5, size = 3, angle = 90),
    axis.text.x = ggplot2::element_text(hjust = 0.5, size = 4)
  ) +
    ggplot2::xlim(0, (data_constraint + batch_size)) +
    ggplot2::ylim(ylim[1], ylim[2]) +
    ggplot2::ggtitle("ECG") +
    ggplot2::ylab("value") +
    ggplot2::xlab(glue_fmt("time {offset/rate:4.1f}s")) # 2.5 is 250hz/batch_size

  return(aa)
}

plot_cac_streaming <- function(arcs, data_constraint, window_size, mp_time_constraint, floss_time_constraint, offset,
                               tail_clip = 0, batch_size = 100, rate = 250, curr_cac_min = NULL) {
  cac_size <- length(arcs)
  cac_idxs <- seq.int(1, cac_size)
  nocac <- is.na(arcs[1]) # if not corrected the initial values will be NA

  if (!nocac && tail_clip > 0) {
    arcs[seq.int(cac_size - tail_clip, cac_size)] <- 1.0
  }

  mid_idx <- data_constraint / 2
  # kumaraswamy_idx <- round(cac_size * 0.6311142)

  if (mp_time_constraint > 0) {
    cac_left_idx <- (data_constraint - mp_time_constraint)
  } else {
    cac_left_idx <- mid_idx
  }

  cac_min_idx <- which.min(arcs[seq.int(cac_left_idx, cac_size)]) + cac_left_idx - 1 # min AC in the last 10 seconds
  cac_min <- arcs[cac_min_idx]

  if (nocac) {
    cac_max <- max(arcs, na.rm = TRUE)
  } else {
    cac_max <- 1
  }

  if (!is.null(curr_cac_min)) {
    if (cac_min < curr_cac_min) {
      curr_cac_min <- cac_min
    }
  } else {
    curr_cac_min <- cac_min
  }

  bb <- ggplot2::ggplot(data.frame(x = cac_idxs, y = arcs), ggplot2::aes(x, y)) +
    ggplot2::geom_line(size = 0.1) +
    ggplot2::theme_grey(base_size = 7) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(hjust = 0.5, size = 3, angle = 90),
      axis.text.x = ggplot2::element_text(hjust = 0.5, size = 4)
    )

  bb <- bb + ggplot2::geom_vline(xintercept = cac_min_idx, color = "red", size = 0.1) +
    ggplot2::annotate("text", x = cac_min_idx + 125, y = cac_min + 0.05, label = glue_fmt("{cac_min:.2f}"), color = "red", size = 1.5) +
    ggplot2::annotate("text", x = cac_min_idx + 125, y = 0.05, label = glue::glue("{cac_min_idx}"), color = "red", size = 1.5) +
    ggplot2::annotate("segment",
      y = curr_cac_min, yend = curr_cac_min, x = mid_idx - 2 * window_size,
      xend = mid_idx + 2 * window_size, color = "blue", size = 0.1
    ) +
    ggplot2::annotate("text", x = mid_idx, y = curr_cac_min + 0.05, label = glue_fmt("{curr_cac_min:.2f}"), color = "blue", size = 1.5)


  bb <- bb + ggplot2::xlim(0, (data_constraint + batch_size)) +
    ggplot2::ylim(0, cac_max) +
    ggplot2::ggtitle("FLOSS") +
    ggplot2::ylab("cac - similarity") +
    ggplot2::xlab(glue_fmt("time {offset/rate:4.1f}s"))

  bb$curr_cac_min <- curr_cac_min

  return(bb)
}

plot_raw_arcs <- function(arcs, iac, data_constraint, mp_time_constraint, floss_time_constraint, offset,
                          batch_size = 100, rate = 250) {
  cac_size <- length(arcs)
  cac_idxs <- seq.int(1, cac_size)
  ymax <- data_constraint * 0.6
  if (mp_time_constraint > 0) {
    ymax <- mp_time_constraint * 0.6
  } else if (floss_time_constraint > 0) {
    ymax <- max(arcs)
  }

  cc <- ggplot2::ggplot(data.frame(x = cac_idxs, y = arcs, z = iac), ggplot2::aes(x, y)) +
    ggplot2::geom_line(size = 0.1) +
    ggplot2::geom_line(ggplot2::aes(x, z), size = 0.1, color = "red") +
    ggplot2::theme_grey(base_size = 7) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(hjust = 0.5, size = 3, angle = 90),
      axis.text.x = ggplot2::element_text(hjust = 0.5, size = 4)
    ) +
    ggplot2::xlim(0, (data_constraint + batch_size)) +
    ggplot2::ylim(0, ymax) +
    ggplot2::ggtitle("FLOSS - ARCS") +
    ggplot2::ylab("cac - similarity") +
    ggplot2::xlab(glue_fmt("time {offset/rate:4.1f}s"))

  return(cc)
}
