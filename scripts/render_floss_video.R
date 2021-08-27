render_floss_video <- function(video_file = here::here("dev", "floss_default.mp4"), ecg_data, arc_counts, title = "Online Semantic Segmentation", framerate = 25, data_constraint = 5000, window_size = 200, batch_size = 100) {
  done <- FALSE

  checkmate::qassert(video_file, "S")
  checkmate::qassert(ecg_data, "N+")
  checkmate::qassert(arc_counts, "L")

  # Register exit point ----------------------
  "!DEBUG Register exit point"
  on.exit(
    if (done) {
      return(invisible(done))
    } else {
      warning("Something went wrong. Anything produced is at ", here::here("tmp"), "/")
      return(invisible(FALSE))
    }
  )

  data_min <- min(ecg_data)
  data_max <- max(ecg_data)
  offset <- data_constraint / 250 # 250hz

  data_idxs <- seq.int(1, data_constraint)
  cac_idxs <- seq.int(1, data_constraint - window_size + 1)
  cac_size <- length(cac_idxs)

  if (!dir.exists(here::here("tmp"))) {
    dir.create(here::here("tmp"), recursive = TRUE, showWarnings = FALSE)
  }

  message("This will take a while:")
  cat("Plotting frames.")

  curr_cac_min <- Inf
  rate <- 250
  mid_idx <- data_constraint / 2 # for 5000 this means 10 seconds
  kumarovski_idx <- round(cac_size * 0.6311142)
  diagnostic_limit_idx <- data_constraint - (10 * 250) # 10 seconds, for physionet
  nocac <- is.na(max(arc_counts[[1]]$cac)) # if not corrected the initial values will be NA
  cac_max <- 1

  for (d in seq_along(arc_counts)) {
    a <- ggplot2::ggplot(data.frame(x = data_idxs, y = ecg_data[(data_idxs + ((d - 1) * batch_size))]), ggplot2::aes(x, y)) +
      ggplot2::geom_line(size = 0.1) +
      ggplot2::theme_grey(base_size = 7) +
      ggplot2::xlim(0, (data_constraint + batch_size)) +
      ggplot2::ylim(data_min, data_max) +
      ggplot2::ggtitle("ECG") +
      ggplot2::ylab("value") +
      ggplot2::xlab(sprintf("time %4.1fs", (d - 1) / 2.5 + offset)) # 2.5 is 250hz/batch_size

    cac_min_idx <- which.min(arc_counts[[d]]$cac[seq.int(kumarovski_idx, cac_size)]) + kumarovski_idx - 1
    cac_min <- arc_counts[[d]]$cac[cac_min_idx]
    if (nocac) {
      cmax <- max(arc_counts[[d]]$cac, na.rm = TRUE)
      if (cmax > cac_max) {
        cac_max <- cmax
      }
    }

    if (cac_min < curr_cac_min) {
      curr_cac_min <- cac_min
    }

    b <- ggplot2::ggplot(data.frame(x = cac_idxs, y = arc_counts[[d]]$cac), ggplot2::aes(x, y)) +
      ggplot2::geom_line(size = 0.1) +
      ggplot2::theme_grey(base_size = 7) +
      ggplot2::geom_vline(xintercept = cac_min_idx, color = "red", size = 0.1) +
      ggplot2::annotate("text", x = cac_min_idx + 125, y = cac_min + 0.1, label = sprintf("%.2f", cac_min), color = "red", size = 1.5) +
      ggplot2::annotate("segment", y = curr_cac_min, yend = curr_cac_min, x = mid_idx - 2 * window_size, xend = mid_idx + 2 * window_size, color = "blue", size = 0.1) +
      ggplot2::annotate("text", x = mid_idx, y = curr_cac_min + 0.1, label = sprintf("%.2f", curr_cac_min), color = "blue", size = 1.5) +
      ggplot2::xlim(0, (data_constraint + batch_size)) +
      ggplot2::ylim(0, cac_max) +
      ggplot2::ggtitle("FLOSS") +
      ggplot2::ylab("cac/similarity") +
      ggplot2::xlab(sprintf("time %4.1fs", (d - 1) / 2.5 + offset))

    gg <- gridExtra::grid.arrange(a, b,
      nrow = 2, newpage = FALSE,
      top = grid::textGrob(title,
        gp = grid::gpar(fontsize = 8)
      )
    )

    ggplot2::ggsave(
      plot = gg, filename = here::here("tmp", sprintf("plot%03d.png", d)), device = "png", width = 5, height = 3, scale = 0.8
    )
    if (!(d %% 10)) {
      cat(".")
    }
  }

  cat("\n")

  message("Starting Magick.")

  library(magick)
  ## list file names and read in
  imgs <- list.files(here::here("tmp"), pattern = "*.png", full.names = TRUE)

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
