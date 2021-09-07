render_floss_video <- function(video_file = here::here("dev", "floss_default.mp4"), ecg_data,
                               arc_counts, title = "Online Semantic Segmentation", framerate = 25, filter_w_size = 0, tail_clip = 0, temp_dir = "tmp", subset = FALSE, sample = FALSE) {
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
      warning("Something went wrong. Anything produced is at ", here::here("tmp", temp_dir), "/")
      return(invisible(FALSE))
    }
  })

  rate <- 250
  last_ten_secs <- 290 * rate # 10 seconds from the end

  params <- attr(arc_counts, "params")

  window_size <- params$window_size
  data_constraint <- params$history
  time_constraint <- params$time_constraint
  batch_size <- arc_counts[[2]]$offset - arc_counts[[1]]$offset

  event_line <- last_ten_secs
  info <- attr(ecg_data, "info")

  if (!is.null(info$filter)) {
    filter <- head(info$filter, last_ten_secs - filter_w_size + 1)
    event_offset <- sum(filter) # total of points skipped by the filter
    event_line <- event_line - event_offset
  }

  data_min <- min(ecg_data)
  data_max <- max(ecg_data)

  data_idxs <- seq.int(1, data_constraint)
  cac_idxs <- seq.int(1, data_constraint - window_size + 1)
  cac_size <- length(cac_idxs)

  if (dir.exists(here::here("tmp", temp_dir))) {
    unlink(here::here("tmp", temp_dir))
  }

  dir.create(here::here("tmp", temp_dir), recursive = TRUE, showWarnings = FALSE)

  message("This will take a while:")
  cat("Plotting frames.")

  curr_cac_min <- Inf
  mid_idx <- data_constraint / 2 # for 5000 this means 10 seconds
  kumarovski_idx <- round(cac_size * 0.6311142)
  nocac <- is.na(max(arc_counts[[1]]$cac)) # if not corrected the initial values will be NA
  curr_cac_max <- 1

  ## *** Parallel messes up with min and max... dont use
  ## cl <- parallel::makePSOCKcluster(4, outfile = "")
  ## doParallel::registerDoParallel(cl)
  ## `%dopar%` <- foreach::`%dopar%`
  ## foreach::foreach(d = seq_along(arc_counts)) %dopar% {



  for (d in seq_along(arc_counts)) {
    if (!isFALSE(subset)) {
      min_s <- min(subset)
      max_s <- max(subset)

      if (!(arc_counts[[d]]$offset <= max_s && arc_counts[[d]]$offset >= min_s)) {
        next
      }
    }

    if (isTRUE(sample)) {
      d <- length(arc_counts) - 10
    }

    data_idxs_subset <- seq.int(arc_counts[[d]]$offset - data_constraint + 1, arc_counts[[d]]$offset)

    aa <- ggplot2::ggplot(data.frame(x = data_idxs, y = ecg_data[data_idxs_subset]), ggplot2::aes(x, y)) +
      ggplot2::geom_line(size = 0.1)

    aa <- aa + ggplot2::annotate("segment", y = data_min, yend = data_min, x = data_constraint - window_size, xend = data_constraint, color = "blue", size = 0.1) +
      ggplot2::annotate("text", x = data_constraint - (window_size / 2), y = data_min * 0.85, label = "Window size", color = "blue", size = 0.8)

    if (arc_counts[[d]]$trigger$trigger_abs_idx > 0) {
      aa <- aa + ggplot2::annotate("segment",
        y = data_min, yend = data_max, x = arc_counts[[d]]$trigger$trigger_abs_idx - (arc_counts[[d]]$offset - data_constraint),
        xend = arc_counts[[d]]$trigger$trigger_abs_idx - (arc_counts[[d]]$offset - data_constraint), color = "blue", size = 0.1
      ) +
        ggplot2::annotate("text", x = data_constraint - (window_size / 2), y = data_min * 0.85, label = "Change", color = "blue", size = 0.8)
    }

    if (arc_counts[[d]]$offset > event_line) {
      curr_event_line <- data_constraint - (arc_counts[[d]]$offset - event_line)
      aa <- aa + ggplot2::annotate("segment", y = data_min, yend = data_max, x = curr_event_line, xend = curr_event_line, color = "blue", size = 0.1) +
        ggplot2::annotate("text", x = curr_event_line - 40, y = data_min * 0.5, label = "Event limit", color = "blue", size = 1, angle = 90)
    }

    if (time_constraint > 0) {
      curr_ts_constr <- (data_constraint - time_constraint)
      aa <- aa + ggplot2::annotate("segment", y = data_min, yend = data_max, x = curr_ts_constr, xend = curr_ts_constr, color = "red", size = 0.1) +
        ggplot2::annotate("text", x = curr_ts_constr - 40, y = data_min * 0.4, label = "Time constraint", color = "red", size = 1, angle = 90)
    }

    aa <- aa + ggplot2::theme_grey(base_size = 7) + ggplot2::theme(
      axis.text.y = ggplot2::element_text(hjust = 0.5, size = 3, angle = 90),
      axis.text.x = ggplot2::element_text(hjust = 0.5, size = 4)
    ) +
      ggplot2::xlim(0, (data_constraint + batch_size)) +
      ggplot2::ylim(data_min, data_max) +
      ggplot2::ggtitle("ECG") +
      ggplot2::ylab("value") +
      ggplot2::xlab(sprintf("time %4.1fs", arc_counts[[d]]$offset / rate)) # 2.5 is 250hz/batch_size

    # cac_min_idx <- which.min(arc_counts[[d]]$cac[seq.int(kumarovski_idx, cac_size)]) + kumarovski_idx - 1

    if (!nocac && tail_clip > 0) {
      arc_counts[[d]]$cac[seq.int(cac_size - tail_clip, cac_size)] <- 1.0
    }

    if (time_constraint > 0) {
      cac_left_idx <- (data_constraint - time_constraint)
    } else {
      cac_left_idx <- mid_idx
    }

    cac_min_idx <- which.min(arc_counts[[d]]$cac[seq.int(cac_left_idx, cac_size)]) + cac_left_idx - 1 # min AC in the last 10 seconds
    cac_min <- arc_counts[[d]]$cac[cac_min_idx]
    if (nocac) {
      cmax <- max(arc_counts[[d]]$cac, na.rm = TRUE)
      if (cmax > curr_cac_max) {
        curr_cac_max <- cmax
      }
    }

    if (cac_min < curr_cac_min) {
      curr_cac_min <- cac_min
    }

    bb<- ggplot2::ggplot(data.frame(x = cac_idxs, y = arc_counts[[d]]$cac), ggplot2::aes(x, y)) +
      ggplot2::geom_line(size = 0.1) +
      ggplot2::theme_grey(base_size = 7) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(hjust = 0.5, size = 3, angle = 90),
        axis.text.x = ggplot2::element_text(hjust = 0.5, size = 4)
      ) +
      ggplot2::geom_vline(xintercept = cac_min_idx, color = "red", size = 0.1) +
      ggplot2::annotate("text", x = cac_min_idx + 125, y = cac_min + 0.1, label = sprintf("%.2f", cac_min), color = "red", size = 1.5) +
      ggplot2::annotate("text", x = cac_min_idx + 125, y = 0.1, label = sprintf("%d", cac_min_idx), color = "red", size = 1.5) +
      ggplot2::annotate("segment", y = curr_cac_min, yend = curr_cac_min, x = mid_idx - 2 * window_size, xend = mid_idx + 2 * window_size, color = "blue", size = 0.1) +
      ggplot2::annotate("text", x = mid_idx, y = curr_cac_min + 0.1, label = sprintf("%.2f", curr_cac_min), color = "blue", size = 1.5) +
      ggplot2::xlim(0, (data_constraint + batch_size)) +
      ggplot2::ylim(0, curr_cac_max) +
      ggplot2::ggtitle("FLOSS") +
      ggplot2::ylab("cac/similarity") +
      ggplot2::xlab(sprintf("time %4.1fs", arc_counts[[d]]$offset / rate))

    cc <- ggplot2::ggplot(data.frame(x = cac_idxs, y = arc_counts[[d]]$arcs), ggplot2::aes(x, y)) +
      ggplot2::geom_line(size = 0.1) +
      ggplot2::theme_grey(base_size = 7) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(hjust = 0.5, size = 3, angle = 90),
        axis.text.x = ggplot2::element_text(hjust = 0.5, size = 4)
      ) +
      ggplot2::xlim(0, (data_constraint + batch_size)) +
      ggplot2::ylim(0, ifelse(time_constraint > 0, time_constraint * 0.6, data_constraint * 0.6)) +
      ggplot2::ggtitle("FLOSS - ARCS") +
      ggplot2::ylab("cac/similarity") +
      ggplot2::xlab(sprintf("time %4.1fs", arc_counts[[d]]$offset / rate))

    dd <- ggplot2::ggplot(data.frame(x = cac_idxs, y = arc_counts[[d]]$iac), ggplot2::aes(x, y)) +
      ggplot2::geom_line(size = 0.1) +
      ggplot2::theme_grey(base_size = 7) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(hjust = 0.5, size = 3, angle = 90),
        axis.text.x = ggplot2::element_text(hjust = 0.5, size = 4)
      ) +
      ggplot2::xlim(0, (data_constraint + batch_size)) +
      ggplot2::ylim(0, ifelse(time_constraint > 0, time_constraint * 0.6, data_constraint * 0.6)) +
      ggplot2::ggtitle("FLOSS - IAC") +
      ggplot2::ylab("cac/similarity") +
      ggplot2::xlab(sprintf("time %4.1fs", arc_counts[[d]]$offset / rate))

    gg <- gridExtra::grid.arrange(aa, bb, cc, dd,
      nrow = 4, newpage = FALSE,
      top = grid::textGrob(title,
        gp = grid::gpar(fontsize = 8)
      )
    )

    if (isTRUE(sample)) {
      ggplot2::ggsave(
        plot = gg, filename = here::here("dev", sprintf("sample_plot%03d.png", d)),
        device = "png", width = 5, height = 3, scale = 0.8
      )

      done <- TRUE
      return(gg)
    }


    ggplot2::ggsave(
      plot = gg, filename = here::here("tmp", temp_dir, sprintf("plot%03d.png", d)),
      device = "png", width = 5, height = 10, scale = 0.8
    )
    if (!(d %% 10)) {
      cat(".")
    }
  }

  cat("\n")

  message("Starting Magick.")

  library(magick)
  ## list file names and read in
  imgs <- list.files(here::here("tmp", temp_dir), pattern = "*.png", full.names = TRUE)

  message("This will take a while:")

  message("- Loading plots.")
  img_list <- lapply(imgs, magick::image_read)

  message("- Merging frames.")
  ## join the images together
  img_joined <- magick::image_join(img_list)

  message("- Encoding the video.")
  magick::image_write_video(img_joined, path = video_file, framerate = framerate)

  message("- Removing temporary files.")
  file.remove(imgs)

  done <- TRUE
  message("Done.")
}
