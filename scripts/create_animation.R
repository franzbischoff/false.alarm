create_animation <- function(ds_objects_names, floss_objects_names, include = c("II", "time", "I", "III", "V", "ABP", "PLETH", "RESP"),
                             exclude = NULL,
                             framerate = 20,
                             subset = FALSE,
                             sample = FALSE) {
  checkmate::qassert(ds_objects_names, "S+")
  checkmate::qassert(floss_objects_names, "S+")
  checkmate::qassert(include, "S+")
  checkmate::qassert(exclude, c("S", "0"))
  checkmate::qassert(framerate, "N[10,40]")
  checkmate::qassert(sample, "B")

  include <- match.arg(include, several.ok = TRUE)
  if (!is.null(exclude)) {
    exclude <- match.arg(include, several.ok = TRUE)
  }

  e <- suppressWarnings(tryCatch(
    source(here::here("scripts", "render_floss_video.R"), encoding = "UTF-8"),
    error = function(e) {
      message("Error: Could not load render script.")
      return(FALSE)
    }
  ))

  if (isFALSE(e)) {
    return(invisible(FALSE))
  } else {
    rm(e)
  }

  # ds_objects_names <- tar_manifest(contains("dataset"), "name")$name
  # floss_objects_names <- tar_manifest(contains("regimes"), "name")$name
  # floss_objects_names <- tar_manifest(contains("mps_floss" & !contains("filtered"), "name")$name
  # ds_objects_names <- tar_manifest(contains("filtered") & !contains("stats"), "name")$name
  # floss_objects_names <- tar_manifest(contains("floss") & contains("filtered"), "name")$name

  for (data_name in ds_objects_names) {
    dataset <- tar_read_raw(data_name)

    filter_w_size <- attr(dataset[[1]], "params")$filter_w_size
    if (!is.null(filter_w_size)) {
      floss_names <- grep(sprintf(".*%d$", filter_w_size), floss_objects_names, value = TRUE)
    } else {
      floss_names <- floss_objects_names
    }

    for (floss_name in floss_names) {
      floss_data <- tar_read_raw(floss_name)

      for (s in seq_along(floss_data)) {
        ds_obj_names <- names(dataset[[s]])
        names <- union(setdiff(ds_obj_names, include), exclude)

        if (!rlang::is_empty(names)) {
          "!DEBUG excluding names `names` from dataset."
          for (n in names) {
            purrr::pluck(dataset[[s]], n) <- NULL
          }
        }

        floss_obj_names <- names(floss_data[[s]])
        names <- union(setdiff(floss_obj_names, include), exclude)

        if (!rlang::is_empty(names)) {
          "!DEBUG excluding names `names` from floss."
          for (n in names) {
            purrr::pluck(floss_data[[s]], n) <- NULL
          }
        }

        params <- attr(floss_data[[s]], "params")
        window_size <- params$window_size ##
        time_constraint <- params$time_constraint ##
        ds_info <- attr(dataset[[s]], "info")
        info <- attr(floss_data[[s]], "info")

        checkmate::assert_true(identical(ds_info$filename, info$filename))

        filename <- info$filename
        alarm <- info$alarm ##
        alarm_true <- info$true ##

        for (k in seq_along(floss_data[[s]])) {
          td_info <- attr(dataset[[s]][[k]], "info")
          t_info <- attr(floss_data[[s]][[k]], "info")

          track <- t_info$label

          if (!is.null(filter_w_size) && filter_w_size != 0) {
            etc <- sprintf("filter_%d", filter_w_size)
          } else {
            filter_w_size <- 0 # TODO: add filter_w_size on ecg_data attributes
            etc <- "raw"
          }

          temp_dir <- sprintf("tmp_%s_%s_%s", filename, track, etc)

          file <- sprintf("%s_%s_%d_%d_%s.mp4", filename, track, window_size, time_constraint, etc)
          title <- sprintf("FLOSS - %s-%s, w: %d, c: %d, %s-%s", filename, track, window_size, time_constraint, alarm, alarm_true)

          message("Rendering file: ", file)
          message("With title: ", title)

          if (file.exists(here::here("dev", "videos", file))) {
            message("Video file already exists: ", file)
          } else {
            a <- render_floss_video(here::here("dev", "videos", file),
              ecg_data = dataset[[s]][[track]], arc_counts = floss_data[[s]][[k]],
              title = title, temp_dir = temp_dir, filter_w_size = filter_w_size,
              framerate = framerate,
              subset = subset,
              sample = sample
            )

            if (isFALSE(a)) {
              return(invisible(FALSE))
            }

            if (isTRUE(sample)) {
              return()
            }
          }
        }
      }

      rm(floss_data)
      gc()
    }

    rm(dataset)
    gc()
  }
}
