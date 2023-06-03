# this should be called by the user
# progressr::handlers(global = TRUE)
# progressr::handlers(progressr::handler_debug)

#' @export
contrast_train_model <- function(truth, ts, ..., window_size, regime_threshold, regime_landmark, mp_threshold, time_constraint) {
  cli::cli_alert(c("!" = "Training the model: <<- this takes time"))
  # cli::cli_inform(c("*" = "contrast_train_model: dots_n {rlang::dots_n(...)}"))
  if (rlang::dots_n(...) > 0L) {
    # 0L
    cli::cli_alert(c("*" = "contrast_train_model: dots_names {names(rlang::dots_list(..., .preserve_empty = TRUE))}"))
  }

  n <- nrow(ts)
  if (n == 0L) {
    rlang::abort("There are zero rows in the predictor set.")
  }

  cli::cli_inform(c("*" = "Training the model: window {window_size}, threshold {mp_threshold}, time constraint {time_constraint}."))
  cli::cli_inform(c("*" = "Training the model: number of recordings {n}."))

  "!DEBUG fitting model."

  if (ncol(ts) > 1L) {
    id <- ts[[1L]]
    ts <- ts[[2L]]
  } else {
    id <- seq.int(1L, n)
  }

  res <- list(truth = truth, id = as.character(id))

  if (foreach::getDoParRegistered()) {
    cli::cli_inform(c("*" = "Training the model: using `furrr`."))
    f_train <- function(ts, window_size, mp_threshold, time_constraint) {
      p <- progressr::progressor(steps = length(ts))
      furrr::future_map(ts,
        function(ts, window_size, mp_threshold, time_constraint) {
          p(glue::glue("Starting."))
          contrast_train_regimes(ts, window_size, mp_threshold, time_constraint)
        }, window_size, mp_threshold, time_constraint,
        .options = furrr::furrr_options(seed = TRUE, scheduling = 1L, packages = "false.alarm")
      )
    }
    contrast <- f_train(ts, window_size, mp_threshold, time_constraint)
  } else {
    cli::cli_inform(c("*" = "Training the model: using `purrr`."))
    contrast <- purrr::map(
      ts,
      contrast_train_regimes,
      window_size,
      mp_threshold,
      time_constraint
    )
  }

  res$contrast <- contrast

  rm(contrast)
  gc(verbose = FALSE)

  trained <- list(
    fitted.values = tibble::as_tibble(res),
    terms = list(
      window_size = window_size,
      mp_threshold = mp_threshold,
      time_constraint = time_constraint,
      regime_threshold = regime_threshold,
      regime_landmark = regime_landmark
    )
  )

  class(trained) <- "contrast_regime_model"

  return(trained)
}

#' @export

contrast_evaluate_all_platos <- function(true_data, false_data, contrast_profiles, quantiles = c(0.1, 1 / 3), segment_size = 2800) {
  checkmate::qassert(true_data, "N+")
  checkmate::qassert(false_data, "N+")
  checkmate::qassert(contrast_profiles, "L+")

  w_sizes <- names(contrast_profiles) # retrieve the window sizes that are stored as list labels
  c_sizes <- as.numeric(w_sizes) # then convert them to numeric

  segs <- list()
  cont <- matrix(Inf, ncol = length(w_sizes), nrow = ncol(contrast_profiles[[1]]$cps))
  thlds <- matrix(Inf, ncol = length(w_sizes), nrow = ncol(contrast_profiles[[1]]$cps))

  for (i in seq_along(c_sizes)) {
    # get all the distance profiles of all top-k platos for the true and false data
    tp <- topk_distance_profiles(true_data, contrast_profiles, c_sizes[i])
    fp <- topk_distance_profiles(false_data, contrast_profiles, c_sizes[i])

    # computes the index of each segment
    segments <- unique(c(seq(0, nrow(tp), by = segment_size), nrow(tp)))

    # number of top-k platos is equal to the ncol of the distance profiles
    knn <- ncol(tp)

    # this matrix is equivalent to "k" vectors of TRUE/FALSE values for each segment
    seg <- matrix(0, ncol = length(segments) - 1, nrow = knn)

    for (k in seq_len(knn)) {
      # get the distance profiles
      tpk <- tp[, k]
      fpk <- fp[, k]

      # get the minimum value on the false data to use as a threshold
      fpk_min <- min(fpk, na.rm = TRUE)
      thlds[k, i] <- fpk_min
      max_min <- NULL

      # iterate over the segments
      for (j in seq_along(segments)) {
        if (j < length(segments)) {
          # get just that segment distance profile
          sm <- tpk[seq(segments[j] + 1, segments[j + 1])]
          # keep only the ones that are smaller than the threshold
          sm <- sm[sm < fpk_min]

          # if there are any value left, we can classify the segment
          if (length(sm) > 0) {
            # get the 10% percentile of the values
            # this gives us a hint of how well the shapelet is doing by segment
            max_min <- c(max_min, quantile(sm, quantiles[1], na.rm = TRUE))
            seg[k, j] <- 1 # store as TRUE, since we have values below the threshold
          }
        }
      }

      # if the plato could classify any segment...
      if (!is.null(max_min)) {
        # computes the overall contrast value using the quantile 1/3 of the max_min values, normalized
        # by the sqrt(2*w) (same factor used on the contrast profile), so we can compare different window sizes
        cont[k, i] <- (fpk_min - quantile(max_min, quantiles[2], na.rm = TRUE)) / sqrt(2 * c_sizes[i])
      } else {
        # if the plato could not classify any segment, we set the contrast to 0
        cont[k, i] <- 0
      }
    }
    segs[[w_sizes[i]]] <- seg
  }

  # here we compute the total number of segments that each plato could classify
  total_counts <- as.matrix(purrr::map_dfr(segs, function(x) apply(x, 1, sum)))
  colnames(cont) <- w_sizes # set the column names on the overall contrast matrix
  colnames(thlds) <- w_sizes # set the column names on the overall contrast matrix

  # NOTE: cont and segs seems to complement each other
  # cont is similar to plot_topk_contrasts (contrasts values by k vs shapelet size)
  # but it is more specific, since it takes into account the segments and not just the
  # best contrast.

  list(
    contrast = cont, # cont == overall contrast of each plato (~specificity)
    coverage = segs, # segs == coverage of each plato (~sensitivity)
    thresholds = thlds, # thlds == threshold of each plato
    cov_counts = total_counts, # sum of segs == 1. Best is sum == num_segments
    num_segments = (length(segments) - 1)
  )
}

#' @export
contrast_train_regimes <- function(ecg_data, window_size, mp_threshold, time_constraint,
                                   ez = 0.5, history = 5000L, sample_freq = 250L, batch = 100L) {
  # cli::cli_inform(c("*" = "Training.\n\n"))
  "!DEBUG processing Stats"

  stats <- compute_companion_stats(ecg_data,
    list(window_size = window_size, n_workers = 1L),
    infos = list(foo = "bar")
  )

  "!DEBUG processing MP"

  mp <- compute_s_profile_with_stats(list(ecg_data, stats),
    params = list(
      window_size = window_size,
      history = history, threshold = mp_threshold,
      mp_time_constraint = time_constraint, progress = FALSE,
      ez = ez, batch = batch
    ),
    infos = list(foo = "bar")
  )

  rm(stats)
  gc(verbose = FALSE)

  "!DEBUG processing FLOSS"

  contrast <- compute_contrast(mp,
    params = list(
      window_size = window_size,
      ez = round(ez * window_size + .Machine$double.eps^0.5),
      mp_time_constraint = time_constraint,
      contrast_time_constraint = 0L,
      cac_only = TRUE,
      history = history,
      sample_freq = sample_freq
    ),
    infos = list(foo = "bar")
  )
  rm(mp)
  gc(verbose = FALSE)

  contrast
}

compute_companion_stats <- function(ecg_data, params, infos) {
  checkmate::qassert(ecg_data, "N+")
  checkmate::qassert(params, "L+")
  checkmate::qassert(infos, "L+")

  data_len <- length(ecg_data)
  msd <- false.alarm::muinvn(ecg_data, params$window_size, params$n_workers)
  avg <- msd$avg
  sig <- msd$sig
  ddf <- -1.0 * diff(ecg_data, lag = params$window_size) / 2.0
  ddg <- (ecg_data[seq.int(params$window_size + 1L, data_len)] - avg[seq.int(2L, data_len - params$window_size + 1L)]) +
    (ecg_data[seq.int(1L, data_len - params$window_size)] - avg[seq.int(1L, data_len - params$window_size)])

  result <- list(avg = avg, sig = sig, ddf = ddf, ddg = ddg)

  attr(result, "info") <- attr(ecg_data, "info")
  attr(result, "params") <- params

  return(result)
}



compute_s_profile_with_stats <- function(data_with_stats, params, infos) {
  checkmate::qassert(data_with_stats, "L2")
  checkmate::assert_true(identical(
    attr(data_with_stats[[1L]], "info"),
    attr(data_with_stats[[2L]], "info")
  ))
  checkmate::qassert(infos, "L+")

  data <- data_with_stats[[1L]]
  data_info <- attr(data, "info")
  subset_start <- ifelse(isFALSE(data_info$subset), 0L, data_info$subset[1L] - 1L)
  stats <- data_with_stats[[2L]]

  # don't compute if the stats are not compatible
  checkmate::assert_true(
    attr(stats, "params")$window_size == params$window_size
  )

  "!DEBUG History `params$history`, batch size `params$batch`"

  profile_len <- params$history - params$window_size + 1L

  initial_data_vector <- seq.int(1L, params$history)
  initial_stats_vector <- seq.int(1L, profile_len)

  initial_stats <- purrr::map(stats, function(x) x[initial_stats_vector])
  initial_stats$ddf[profile_len] <- 0.0
  initial_stats$ddg[profile_len] <- 0.0

  initial_mp <- list(w = params$window_size, ez = params$ez, offset = subset_start)
  current_mp <- false.alarm::mpx_stream_s_right(data[initial_data_vector],
    batch_size = params$history, initial_mp,
    initial_stats, history = 0L, mp_time_constraint = params$mp_time_constraint, progress = params$progress,
    threshold = params$threshold
  )

  new_data_vector <- seq.int(params$history + 1L, length(data))

  "!DEBUG Data Size `length(data)`."

  new_data_list <- split(new_data_vector, ceiling(seq_along(new_data_vector) / params$batch))

  "!DEBUG Data List Size `length(new_data_list)`."

  profiles <- list()
  profiles[[1L]] <- current_mp
  profiles[[1L]]$motif_quality <- false.alarm::motif_quality(current_mp$right_matrix_profile,
    input_format = "pearson", window_size = params$window_size
  )
  i <- 2L

  for (n in new_data_list) {
    batch <- length(n)
    start <- (current_mp$offset - subset_start) - params$history + 1L
    end <- (current_mp$offset - subset_start) + batch
    data_vector <- seq.int(start, end)
    profile_len <- length(data_vector) - params$window_size + 1L
    stats_vector <- seq.int(start, start + profile_len - 1L)
    current_stats <- purrr::map(stats, function(x) x[stats_vector])
    current_stats$ddf[profile_len] <- 0.0
    current_stats$ddg[profile_len] <- 0.0
    current_mp <- false.alarm::mpx_stream_s_right(data[data_vector],
      batch_size = batch, current_mp,
      current_stats, history = params$history,
      mp_time_constraint = params$mp_time_constraint,
      progress = params$progress,
      threshold = params$threshold
    )

    profiles[[i]] <- current_mp
    profiles[[i]]$motif_quality <- false.alarm::motif_quality(current_mp$right_matrix_profile,
      input_format = "pearson", window_size = params$window_size
    )
    i <- i + 1L
    "!!DEBUG batch size `batch`"
  }

  "!DEBUG Finished `length(profiles)` profiles."

  checkmate::assert_true(
    current_mp$offset == length(data) + subset_start
  )

  attr(profiles, "info") <- data_info

  return(profiles)
}



compute_contrast <- function(mp_data, params, infos) {
  checkmate::qassert(mp_data, "L")
  checkmate::qassert(params, "L+")
  checkmate::qassert(infos, "L+")

  ez <- params$ez
  mp_time_constraint <- ifelse(is.null(params$mp_time_constraint), 0L, params$mp_time_constraint)
  contrast_time_constraint <- ifelse(is.null(params$contrast_time_constraint), 0L, params$contrast_time_constraint)
  if (mp_time_constraint > floor(params$history * 3.0 / 4.0)) {
    mp_time_constraint <- 0L
  }
  sample_freq <- params$sample_freq

  checkmate::qassert(ez, c("0", "N"))

  if (mp_time_constraint > 0L && contrast_time_constraint > 0L) {
    rlang::abort("You cannot set `mp_time_constraint` and `contrast_time_constraint` at the same time.")
  }

  constraint <- FALSE

  if (mp_time_constraint > 0L || contrast_time_constraint > 0L) {
    constraint <- TRUE
  }

  info <- attr(mp_data, "info")

  if (constraint) {
    iac <- vector("list", 500L)
    pro_size <- length(mp_data[[1L]]$right_profile_index)
    for (i in 1L:500L) {
      iac[[i]] <- get_asym(pro_size, mp_time_constraint, contrast_time_constraint)
    }

    aic_avg <- rowMeans(as.data.frame(iac))
    if (mp_time_constraint > 0L && mp_time_constraint < (pro_size / 2.0)) {
      aic_avg[seq.int(mp_time_constraint, pro_size - mp_time_constraint * 0.9)] <- mp_time_constraint / 2.0
    }
  } else {
    aic_avg <- NULL
  }

  result_contrast <- purrr::map(mp_data, function(x) {
    curr_ez <- ifelse(!is.null(ez), ez, x$ez * 10.0)
    cac <- compute_arcs(
      x$right_profile_index, x$w,
      curr_ez,
      aic_avg,
      sample_freq,
      contrast_time_constraint
    )

    if (isTRUE(params$cac_only)) {
      list(cac = cac$cac, w = x$w, ez = curr_ez, offset = x$offset)
    } else {
      list(cac = cac$cac, iac = cac$iac, arcs = cac$arcs, w = x$w, ez = curr_ez, offset = x$offset)
    }
  })

  "!DEBUG Finished `length(result_contrast)` profiles."

  attr(result_contrast, "info") <- info
  attr(result_contrast, "params") <- params

  return(result_contrast)
}

compute_arcs <- function(right_profile_index, window_size, exclusion_zone, aic_avg, sample_freq, contrast_time_constraint) {
  checkmate::qassert(right_profile_index, "N")
  checkmate::qassert(exclusion_zone, "N")
  checkmate::assert_true(sample_freq > 50L)

  "!!DEBUG Compute ARCS"

  ez <- round(exclusion_zone + .Machine$double.eps^0.5)
  arc_counts <- vector(mode = "numeric")
  cac_size <- length(right_profile_index)
  nnmark <- vector(mode = "numeric", cac_size)

  if (contrast_time_constraint > 0L) {
    constraint <- contrast_time_constraint
  } else {
    constraint <- cac_size
  }

  for (i in seq.int(1L, (cac_size - ez - 1L))) {
    j <- right_profile_index[i]

    if (abs(j - i) <= constraint) {
      if (j == i) {
        next
      }

      if (j < 0L || j > cac_size) {
        next
      }

      nnmark[min(i, j)] <- nnmark[min(i, j)] + 1L
      nnmark[max(i, j)] <- nnmark[max(i, j)] - 1L
    }
  }

  arc_counts <- cumsum(nnmark)
  iac <- NULL

  if (!is.null(aic_avg)) {
    iac <- aic_avg
    cac <- pmin(arc_counts / iac, 1.0) # below 1 or 1
    cac[seq.int(1L, (window_size / 2.0))] <- 1.0
    cac[seq.int((cac_size - window_size), cac_size)] <- 1.0
    cac[cac < 0.0 | is.na(cac)] <- 1.0
  } else {
    x <- seq(0.0, 1.0, length.out = cac_size)
    # mode <- 0.6311142 # best point to analyze the segment change
    a <- 1.939274
    b <- 1.69815
    iac <- a * b * x^(a - 1.0) * (1.0 - x^a)^(b - 1.0) * cac_size / 4.035477 # nolint # kumaraswamy distribution

    cac <- pmin(arc_counts / iac, 1.0) # below 1 or 1
    cac[seq.int(1L, (window_size / 2.0))] <- 1.0
    cac[seq.int((cac_size - window_size), cac_size)] <- 1.0
    cac[cac < 0.0 | is.na(cac)] <- 1.0
  }

  return(list(arcs = arc_counts, iac = iac, cac = cac))
}


get_asym <- function(pro_len = 50000L, mp_tc = 0L, contrast_tc = 0L) {
  mpi <- rep(0L, pro_len)
  tc <- pro_len

  if (mp_tc > 0L) {
    for (i in (1L:(pro_len - 1L))) {
      mpi[i] <- runif(1L, i + 1.0, min(pro_len, i + mp_tc))
    }
  } else {
    # the same as for pure FLOSS without constraint. The constraint will be done later
    for (i in (1L:(pro_len - 1L))) {
      mpi[i] <- runif(1L, i + 1.0, pro_len)
    }
    tc <- contrast_tc
  }

  nnmark <- rep(0L, pro_len)

  for (i in seq.int(1L, pro_len - 1L)) {
    j <- mpi[i]

    if (abs(j - i) <= tc) {
      if (j == i) {
        next
      }

      if (j < 0L || j > pro_len) {
        next
      }

      nnmark[min(i, j)] <- nnmark[min(i, j)] + 1L
      nnmark[max(i, j)] <- nnmark[max(i, j)] - 1L
    }
  }

  arc_counts <- cumsum(nnmark)
  arc_counts[pro_len] <- 0.0

  return(arc_counts)
}
