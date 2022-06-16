
# this should be called by the user
# progressr::handlers(global = TRUE)
# progressr::handlers(progressr::handler_debug)

#' @export
floss_train_model <- function(truth, ts, ..., window_size, regime_threshold, regime_landmark, mp_threshold, time_constraint) {
  cli::cli_alert(c("!" = "Training the model: <<- this takes time"))
  # cli::cli_inform(c("*" = "floss_train_model: dots_n {rlang::dots_n(...)}"))
  if (rlang::dots_n(...) > 0) { # 0
    cli::cli_alert(c("*" = "floss_train_model: dots_names {names(rlang::dots_list(..., .preserve_empty = TRUE))}"))
  }

  n <- nrow(ts)
  if (n == 0) {
    rlang::abort("There are zero rows in the predictor set.")
  }

  cli::cli_inform(c("*" = "Training the model: window {window_size}, threshold {mp_threshold}, time constraint {time_constraint}."))
  cli::cli_inform(c("*" = "Training the model: number of recordings {n}."))

  "!DEBUG fitting model."

  if (ncol(ts) > 1) {
    id <- ts[[1]]
    ts <- ts[[2]]
  } else {
    id <- seq.int(1, n)
  }

  res <- list(truth = truth, id = as.character(id))

  if (foreach::getDoParRegistered()) {
    cli::cli_inform(c("*" = "Training the model: using `furrr`."))
    f_train <- function(ts, window_size, mp_threshold, time_constraint) {
      p <- progressr::progressor(steps = length(ts))
      furrr::future_map(ts,
        function(ts, window_size, mp_threshold, time_constraint) {
          p(glue::glue("Starting."))
          floss_train_regimes(ts, window_size, mp_threshold, time_constraint)
        }, window_size, mp_threshold, time_constraint,
        .options = furrr::furrr_options(seed = TRUE, scheduling = 1, packages = "false.alarm")
      )
    }
    floss <- f_train(ts, window_size, mp_threshold, time_constraint)
  } else {
    cli::cli_inform(c("*" = "Training the model: using `purrr`."))
    floss <- purrr::map(
      ts,
      floss_train_regimes,
      window_size,
      mp_threshold,
      time_constraint
    )
  }

  res$floss <- floss

  rm(floss)
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

  class(trained) <- "floss_regime_model"

  return(trained)
}

#' @export
floss_train_regimes <- function(ecg_data, window_size, mp_threshold, time_constraint,
                                ez = 0.5, history = 5000, sample_freq = 250, batch = 100) {
  # cli::cli_inform(c("*" = "Training.\n\n"))
  "!DEBUG processing Stats"

  stats <- compute_companion_stats(ecg_data,
    list(window_size = window_size, n_workers = 1),
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

  floss <- compute_floss(mp,
    params = list(
      window_size = window_size,
      ez = round(ez * window_size + .Machine$double.eps^0.5),
      mp_time_constraint = time_constraint,
      floss_time_constraint = 0,
      cac_only = TRUE,
      history = history,
      sample_freq = sample_freq
    ),
    infos = list(foo = "bar")
  )
  rm(mp)
  gc(verbose = FALSE)

  floss
}

compute_companion_stats <- function(ecg_data, params, infos) {
  checkmate::qassert(ecg_data, "N+")
  checkmate::qassert(params, "L+")
  checkmate::qassert(infos, "L+")

  data_len <- length(ecg_data)
  msd <- false.alarm::muinvn(ecg_data, params$window_size, params$n_workers)
  avg <- msd$avg
  sig <- msd$sig
  ddf <- -1 * diff(ecg_data, lag = params$window_size) / 2
  ddg <- (ecg_data[seq.int(params$window_size + 1, data_len)] - avg[seq.int(2, data_len - params$window_size + 1)]) +
    (ecg_data[seq.int(1, data_len - params$window_size)] - avg[seq.int(1, data_len - params$window_size)])

  result <- list(avg = avg, sig = sig, ddf = ddf, ddg = ddg)

  attr(result, "info") <- attr(ecg_data, "info")
  attr(result, "params") <- params

  return(result)
}



compute_s_profile_with_stats <- function(data_with_stats, params, infos) {
  checkmate::qassert(data_with_stats, "L2")
  checkmate::assert_true(identical(
    attr(data_with_stats[[1]], "info"),
    attr(data_with_stats[[2]], "info")
  ))
  checkmate::qassert(infos, "L+")

  data <- data_with_stats[[1]]
  data_info <- attr(data, "info")
  subset_start <- ifelse(isFALSE(data_info$subset), 0, data_info$subset[1] - 1)
  stats <- data_with_stats[[2]]

  # don't compute if the stats are not compatible
  checkmate::assert_true(
    attr(stats, "params")$window_size == params$window_size
  )

  "!DEBUG History `params$history`, batch size `params$batch`"

  profile_len <- params$history - params$window_size + 1

  initial_data_vector <- seq.int(1, params$history)
  initial_stats_vector <- seq.int(1, profile_len)

  initial_stats <- purrr::map(stats, function(x) x[initial_stats_vector])
  initial_stats$ddf[profile_len] <- 0
  initial_stats$ddg[profile_len] <- 0

  initial_mp <- list(w = params$window_size, ez = params$ez, offset = subset_start)
  current_mp <- false.alarm::mpx_stream_s_right(data[initial_data_vector],
    batch_size = params$history, initial_mp,
    initial_stats, history = 0, mp_time_constraint = params$mp_time_constraint, progress = params$progress,
    threshold = params$threshold
  )

  new_data_vector <- seq.int(params$history + 1, length(data))

  "!DEBUG Data Size `length(data)`."

  new_data_list <- split(new_data_vector, ceiling(seq_along(new_data_vector) / params$batch))

  "!DEBUG Data List Size `length(new_data_list)`."

  profiles <- list()
  profiles[[1]] <- current_mp
  profiles[[1]]$motif_quality <- false.alarm::motif_quality(current_mp$right_matrix_profile,
    input_format = "pearson", window_size = params$window_size
  )
  i <- 2

  for (n in new_data_list) {
    batch <- length(n)
    start <- (current_mp$offset - subset_start) - params$history + 1
    end <- (current_mp$offset - subset_start) + batch
    data_vector <- seq.int(start, end)
    profile_len <- length(data_vector) - params$window_size + 1
    stats_vector <- seq.int(start, start + profile_len - 1)
    current_stats <- purrr::map(stats, function(x) x[stats_vector])
    current_stats$ddf[profile_len] <- 0
    current_stats$ddg[profile_len] <- 0
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
    i <- i + 1
    "!!DEBUG batch size `batch`"
  }

  "!DEBUG Finished `length(profiles)` profiles."

  checkmate::assert_true(
    current_mp$offset == length(data) + subset_start
  )

  attr(profiles, "info") <- data_info

  return(profiles)
}



compute_floss <- function(mp_data, params, infos) {
  checkmate::qassert(mp_data, "L")
  checkmate::qassert(params, "L+")
  checkmate::qassert(infos, "L+")

  ez <- params$ez
  mp_time_constraint <- ifelse(is.null(params$mp_time_constraint), 0, params$mp_time_constraint)
  floss_time_constraint <- ifelse(is.null(params$floss_time_constraint), 0, params$floss_time_constraint)
  sample_freq <- params$sample_freq

  checkmate::qassert(ez, c("0", "N"))

  if (mp_time_constraint > 0 & floss_time_constraint > 0) {
    rlang::abort("You cannot set `mp_time_constraint` and `floss_time_constraint` at the same time.")
  }

  constraint <- FALSE

  if (mp_time_constraint > 0 || floss_time_constraint > 0) {
    constraint <- TRUE
  }

  info <- attr(mp_data, "info")

  if (constraint) {
    iac <- vector("list", 500)
    pro_size <- length(mp_data[[1]]$right_profile_index)
    for (i in 1:500) {
      iac[[i]] <- get_asym(pro_size, mp_time_constraint, floss_time_constraint)
    }

    aic_avg <- rowMeans(as.data.frame(iac))
    if (mp_time_constraint > 0 & mp_time_constraint < (pro_size / 2)) {
      aic_avg[seq.int(mp_time_constraint, pro_size - mp_time_constraint * 0.9)] <- mp_time_constraint / 2
    }
  } else {
    aic_avg <- NULL
  }

  result_floss <- purrr::map(mp_data, function(x) {
    curr_ez <- ifelse(!is.null(ez), ez, x$ez * 10)
    cac <- compute_arcs(
      x$right_profile_index, x$w,
      curr_ez,
      aic_avg,
      sample_freq,
      floss_time_constraint
    )
    list(cac = cac$cac, iac = cac$iac, arcs = cac$arcs, w = x$w, ez = curr_ez, offset = x$offset)
  })

  "!DEBUG Finished `length(result_floss)` profiles."

  attr(result_floss, "info") <- info
  attr(result_floss, "params") <- params

  return(result_floss)
}

compute_arcs <- function(right_profile_index, window_size, exclusion_zone, aic_avg, sample_freq, floss_time_constraint) {
  checkmate::qassert(right_profile_index, "N")
  checkmate::qassert(exclusion_zone, "N")
  checkmate::assert_true(sample_freq > 50)

  "!!DEBUG Compute ARCS"

  ez <- round(exclusion_zone + .Machine$double.eps^0.5)
  arc_counts <- vector(mode = "numeric")
  cac_size <- length(right_profile_index)
  nnmark <- vector(mode = "numeric", cac_size)

  if (floss_time_constraint > 0) {
    constraint <- floss_time_constraint
  } else {
    constraint <- cac_size
  }

  for (i in seq.int(1, (cac_size - ez - 1))) {
    j <- right_profile_index[i]

    if (abs(j - i) <= constraint) {
      if (j == i) {
        next
      }

      if (j < 0 || j > cac_size) {
        next
      }

      nnmark[min(i, j)] <- nnmark[min(i, j)] + 1
      nnmark[max(i, j)] <- nnmark[max(i, j)] - 1
    }
  }

  arc_counts <- cumsum(nnmark)
  iac <- NULL

  if (!is.null(aic_avg)) {
    iac <- aic_avg
    cac <- pmin(arc_counts / iac, 1) # below 1 or 1
    cac[seq.int(1, (window_size / 2))] <- 1.0
    cac[seq.int((cac_size - window_size), cac_size)] <- 1.0
    cac[cac < 0 | is.na(cac)] <- 1.0
  } else {
    x <- seq(0, 1, length.out = cac_size)
    # mode <- 0.6311142 # best point to analyze the segment change
    a <- 1.939274
    b <- 1.69815
    iac <- a * b * x^(a - 1) * (1 - x^a)^(b - 1) * cac_size / 4.035477 # nolint # kumaraswamy distribution

    cac <- pmin(arc_counts / iac, 1) # below 1 or 1
    cac[seq.int(1, (window_size / 2))] <- 1.0
    cac[seq.int((cac_size - window_size), cac_size)] <- 1.0
    cac[cac < 0 | is.na(cac)] <- 1.0
  }

  return(list(arcs = arc_counts, iac = iac, cac = cac))
}


get_asym <- function(pro_len = 50000, mp_tc = 0, floss_tc = 0) {
  mpi <- rep(0, pro_len)
  tc <- pro_len

  if (mp_tc > 0) {
    for (i in (1:(pro_len - 1))) {
      mpi[i] <- runif(1, i + 1, min(pro_len, i + mp_tc))
    }
  } else {
    # the same as for pure FLOSS without constraint. The constraint will be done later
    for (i in (1:(pro_len - 1))) {
      mpi[i] <- runif(1, i + 1, pro_len)
    }
    tc <- floss_tc
  }

  nnmark <- rep(0, pro_len)

  for (i in seq.int(1, pro_len - 1)) {
    j <- mpi[i]

    if (abs(j - i) <= tc) {
      if (j == i) {
        next
      }

      if (j < 0 || j > pro_len) {
        next
      }

      nnmark[min(i, j)] <- nnmark[min(i, j)] + 1
      nnmark[max(i, j)] <- nnmark[max(i, j)] - 1
    }
  }

  arc_counts <- cumsum(nnmark)
  arc_counts[pro_len] <- 0

  return(arc_counts)
}
