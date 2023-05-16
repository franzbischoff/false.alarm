# pan <- function(split, shapelet_sizes, progress = FALSE) {
#   checkmate::qassert(split, "L4")
#   checkmate::qassert(shapelet_sizes, "N+")

#   "!DEBUG Compute Pan CP"

#   idxs <- (split$data$alarm == "true")

#   true_alarms <- unlist(split$data$values[idxs])
#   true_alarms <- as.numeric(true_alarms[!is.na(true_alarms)])


#   false_alarms <- unlist(split$data$values[!idxs])
#   false_alarms <- as.numeric(false_alarms[!is.na(false_alarms)])

#   result <- list()

#   for (i in shapelet_sizes) {
#     true_alarms_val <- validate_data(true_alarms, i %/% 2)
#     false_alarms_val <- validate_data(false_alarms, i %/% 2)
#     res <- contrast(false_alarms_val, true_alarms_val, i, progress = progress)
#     result[[as.character(i)]] <- res
#   }

#   return(result)
# }

## sequence:
# res <- readRDS("dev/topk.rds"); sols <- res$sols; res <- res$res
# source("/workspace/false.alarm/scripts/classification/pan_contrast.R", encoding = "UTF-8")
# source("/workspace/false.alarm/scripts/helpers/plot_contrast.R", encoding = "UTF-8")
# source("/workspace/false.alarm/scripts/helpers/pan_contrast_helpers.R", encoding = "UTF-8")
# library(false.alarm)
# score <- score_by_segment_window(res$positive, res$negative, res$pan)
# solutions <- find_solutions(score, cov = 18, n = 10, rep = 100000, red = 10, n_jobs = 2); View(solutions)
# jj <- filter_best_solutions(solutions, 2)
# plt <- plot_best_candidates(jj$data[[1]], res)

# bb %>% dplyr::select(2:7) %>% dplyr::filter(coverage < 20) %>% ggpairs(aes(alpha = 0.05), lower = list(continuous = "smooth"))

# samples vs redundance
# coverage x redundance
# sd vs mean

# get_pan_platos <- function(contrast_profiles, position = 1) {
#   checkmate::qassert(contrast_profiles, "L+")

#   "!DEBUG Get First Pan CP"
#   w_sizes <- names(contrast_profiles)
#   n_sizes <- as.numeric(w_sizes)
#   max_size <- max(n_sizes)

#   pan_platos <- matrix(NA, ncol = length(w_sizes), nrow = max_size)

#   for (i in seq_len(length(w_sizes))) {
#     padded <- c(contrast_profiles[[i]]$platos[, position], rep(NA, max_size - n_sizes[i]))
#     pan_platos[, i] <- padded
#   }

#   colnames(pan_platos) <- w_sizes
#   return(pan_platos)
# }

score_by_segment_window <- function(true_data, false_data, contrast_profiles) {
  checkmate::qassert(true_data, "N+")
  checkmate::qassert(false_data, "N+")
  checkmate::qassert(contrast_profiles, "L+")

  # browser()

  w_sizes <- names(contrast_profiles)
  c_sizes <- as.numeric(w_sizes)

  segs <- list()
  cont <- matrix(Inf, ncol = length(w_sizes), nrow = ncol(contrast_profiles[[1]]$cps))

  for (i in seq_along(c_sizes)) {
    tp <- topk_distance_profiles(true_data, contrast_profiles, c_sizes[i])
    fp <- topk_distance_profiles(false_data, contrast_profiles, c_sizes[i])

    segments <- unique(c(seq(0, nrow(tp), by = 2800), nrow(tp)))

    knn <- ncol(tp)

    seg <- matrix(0, ncol = length(segments) - 1, nrow = knn)

    for (k in seq_len(knn)) {
      tpk <- tp[, k]
      # if (k > 1) {
      #   tpk <- apply(tpk, 1, min)
      # }
      fpk <- fp[, k]
      fpk_min <- min(fpk, na.rm = TRUE)
      max_min <- NULL

      for (j in seq_along(segments)) {
        if (j < length(segments)) {
          sm <- tpk[seq(segments[j] + 1, segments[j + 1])]
          sm <- sm[sm < fpk_min] # filter out the ones that are bigger than the min

          if (length(sm) > 0) {
            # get the 10% percentile of the values
            # this gives us a hint of how well the shapelet is doing by segment
            max_min <- c(max_min, quantile(sm, 0.1, na.rm = TRUE))
            seg[k, j] <- 1
          }
        }
      }

      if (!is.null(max_min)) {
        # stores the overall contrast of the shapelet across segments
        cont[k, i] <- (fpk_min - quantile(max_min, 1 / 3, na.rm = TRUE)) / sqrt(2 * c_sizes[i])
      } else {
        cont[k, i] <- 0
      }
    }
    segs[[w_sizes[i]]] <- seg
  }

  total_counts <- as.matrix(purrr::map_dfr(segs, function(x) apply(x, 1, sum)))
  colnames(cont) <- w_sizes

  # NOTE: cont and segs seems to complement each other
  # cont is similar to plot_topk_contrasts (contrasts values by k vs shapelet size)
  # but it is more specific, since it takes into account the segments and not just the
  # best contrast.

  # cont == best overall contrast (specific)
  # segs == best coverage by segment (sensitive)
  list(contrast = cont, coverage = segs, cov_counts = total_counts, num_segments = (length(segments) - 1))
}

score_candidates <- function(score) {
  checkmate::qassert(score, "L+")
  data <- NULL

  for (i in seq_len(nrow(score$contrast))) {
    for (j in names(score$coverage)) {
      aa <- tibble::tibble_row(
        window = j,
        k = i,
        contrast = score$contrast[i, j],
        cov_sum = sum(score$coverage[[j]][i, ]),
        cov_idxs = list(score$coverage[[j]][i, ])
      )
      data <- dplyr::bind_rows(data, aa)
    }
  }

  data <- data |>
    dplyr::filter(cov_sum > 1, contrast > 0.1) |>
    dplyr::mutate(cov_con = log10(cov_sum) * (1 / -log(contrast)), .before = cov_idxs)

  return(data)
}


filter_best_solutions <- function(solutions, n_sols = 1) {
  best <- solutions |>
    dplyr::filter(coverage == max(coverage, na.rm = TRUE)) |>
    dplyr::filter(c_median > max(c_median, na.rm = TRUE) / 2) |>
    dplyr::filter(c_sd > max(c_sd, na.rm = TRUE) / 2) |>
    dplyr::filter(redundance == min(redundance, na.rm = TRUE)) |>
    dplyr::arrange(c_sd) |>
    dplyr::filter(samples == min(samples, na.rm = TRUE)) |>
    dplyr::slice_head(n = n_sols)

  return(best)
}


find_solutions <- function(score, n = 5, rep = 2000, red = 5, cov = 17, n_jobs = 1) {
  if (cov > score$num_segments) {
    cov <- score$num_segments
  }

  score <- score_candidates(score)

  if (n_jobs == 1) {
    future::plan(future::sequential)
  } else {
    future::plan(future::multisession, workers = n_jobs)
  }

  reps <- seq_len(rep)

  cli::cli_alert_info("Finding solutions using {n_jobs} jobs.")

  progressr::with_progress(
    {
      p <- progressr::progressor(along = reps)

      sols <- furrr::future_map(reps, function(i) {
        p()
        samples <- round(runif(1, 1, n))
        some_rows <- dplyr::slice_sample(score, n = samples)
        n_cols <- length(some_rows$cov_idxs[[1]])
        sample_data <- matrix(unlist(some_rows$cov_idxs), ncol = n_cols, byrow = TRUE)
        data_coverage <- apply(sample_data, 2, sum)

        redundance <- sum(data_coverage > 1)
        if (redundance > red) {
          return(NULL)
        }

        coverage <- sum(data_coverage > 0)
        if (coverage < cov) {
          return(NULL)
        }

        keep_rows <- list()

        if (nrow(sample_data) > 2) {
          # test for rows of samples that don't zero the column sum and may be dropped
          cc <- data_coverage
          for (j in seq((nrow(sample_data) - 1), 2)) {
            comb <- combn(seq_len(nrow(sample_data)), j)

            for (k in seq_len(ncol(comb))) {
              rr <- sample_data[comb[, k], , drop = FALSE]
              rr <- apply(rr, 2, sum)
              if (!any(rr[(rr & cc)] == cc[(rr & cc)])) {
                keep <- setdiff(seq_len(nrow(sample_data)), comb[, k])
                keep_rows <- c(keep_rows, list(keep))
              }
            }
            if (length(keep_rows) > 0) {
              break
            }
          }
        }

        if (length(keep_rows) == 1) {
          some_rows <- some_rows[keep_rows[[1]], ]
          samples <- length(keep_rows[[1]])
          data_coverage <- apply(matrix(unlist(some_rows$cov_idxs), ncol = n_cols, byrow = TRUE), 2, sum)
          redundance <- sum(data_coverage > 1)
          coverage <- sum(data_coverage > 0)
        } else if (length(keep_rows) > 1) {
          new_rows <- NULL
          for (j in seq_len(length(keep_rows))) {
            temp <- dplyr::bind_cols(idx = j, some_rows[keep_rows[[j]], ])
            new_rows <- dplyr::bind_rows(new_rows, temp)
          }

          new_rows <- new_rows |>
            dplyr::group_by(idx) |>
            dplyr::summarise(
              sk = sum(k), mc = median(contrast),
              sid = sum(unlist(cov_idxs)),
              redun = sum(apply(matrix(unlist(cov_idxs), ncol = n_cols, byrow = TRUE), 2, sum) > 1)
            )
          new_rows <- new_rows |>
            dplyr::arrange(sk, desc(mc), sid, redun) |>
            dplyr::slice_head(n = 1)
          new_idx <- new_rows |> dplyr::pull(idx)
          some_rows <- some_rows[keep_rows[[new_idx]], ]

          samples <- length(keep_rows[[new_idx]])
          data_coverage <- apply(matrix(unlist(some_rows$cov_idxs), ncol = n_cols, byrow = TRUE), 2, sum)
          redundance <- sum(data_coverage > 1)
          coverage <- sum(data_coverage > 0)
        }

        # ties: < sum(k), > median(contrast), < sum(idxs), < sum(data_coverage > 1), < sum(redundance)

        # cli::cli_alert_info("sols i={i}, length(keep_rows) {length(keep_rows)}, length(keep_rows[[1]]) {length(keep_rows[[1]])}.")

        # sols[[i]] <- list(
        list(
          c_total = sum(some_rows$contrast), c_median = median(some_rows$contrast), c_mean = mean(some_rows$contrast),
          cov_mean = mean(some_rows$cov_con),
          c_sd = ifelse(length(some_rows$contrast) > 1, sd(some_rows$contrast, na.rm = TRUE), 0), coverage = coverage, redundance = redundance,
          samples = samples, cand = some_rows
        )
      }, .options = furrr::furrr_options(seed = 2023, scheduling = 1))
    },
    delay_terminal = TRUE,
    handlers = progressr::handler_progress(format = "[:bar] :percent :eta :message", interval = 10) # intrusiveness = 100,
  )

  # browser()
  sols <- list_dfr(sols)

  if (length(sols) == 0) {
    cli::cli_warn("No solution found. Try decreasing coverage value.")
  } else {
    sols <- tidyr::unnest(sols, cols = cand) |>
      dplyr::distinct_all() |>
      tidyr::nest(data = c(window, k, contrast, cov_sum, cov_con, cov_idxs))
    cli::cli_alert_info("done.")
  }

  return(sols)
}

list_dfr <- function(x) {
  x <- purrr::map(x, function(x) {
    x <- tibble::as_tibble(x)
    x
  }) |> purrr::list_rbind()

  return(x)
}

list_dfc <- function(x) {
  x <- purrr::map(x, function(x) {
    x <- tibble::as_tibble(x)
    x
  }) |> purrr::list_cbind()

  return(x)
}



# computes the distance profile of all given platos against the given data
topk_distance_profiles <- function(data, contrast_profiles, window_size) {
  checkmate::qassert(contrast_profiles, "L+")

  w_sizes <- names(contrast_profiles)
  w_size <- as.character(window_size)

  if (!(w_size %in% w_sizes)) {
    cli::cli_abort("Invalid window_size.")
  }

  platos <- contrast_profiles[[w_size]]$platos
  platos_twin <- contrast_profiles[[w_size]]$platos_twin

  dps <- matrix(NA, ncol = ncol(platos), nrow = length(data) - window_size + 1)

  for (i in seq_len(ncol(platos))) {
    query <- (platos[, i] + platos_twin[, i]) / 2

    dp <- dist_profile(data, query)
    dps[, i] <- dp
  }

  return(dps)
}


# Output:
#   plato: The subsequence that most distinguishes
#          positiveTS from negativeTS
#   plato_indices: The starting index of each of the K Platos
#   plato_primary_contrast: Contrast value of each plato in the K=1 CP
#   plato_nary_contrast: Contrast value of each plato after appending
#     the previous Plato to negativeTS. May be helpful in identifying
#     diminishing returns and redundant behaviors.
# TODO: check find_k_shapelets()
contrastprofile_topk <- function(split, shapelet_sizes, num_shapelets, n_jobs = 1, progress = FALSE) {
  checkmate::qassert(split, "L4")
  checkmate::qassert(shapelet_sizes, "N+")
  require(doFuture)

  "!DEBUG Compute Pan CP"

  idxs <- (split$data$alarm == "true")

  true_alarms <- NULL
  false_alarms <- NULL

  for (i in seq_along(idxs)) {
    if (isTRUE(idxs[i])) {
      true_alarms <- c(true_alarms, rnorm(300), split$data$values[[i]])
    } else {
      false_alarms <- c(false_alarms, rnorm(300), split$data$values[[i]])
    }
  }

  # time series containing at least two instances of a desired behavior
  true_alarms <- as.numeric(true_alarms[!is.na(true_alarms)])

  # time series containing zero instances of a desired behavior
  false_alarms <- as.numeric(false_alarms[!is.na(false_alarms)])

  true_alarms_val <- validate_data(true_alarms, 20)
  false_alarms_val <- validate_data(false_alarms, 20)

  if (n_jobs == 1) {
    future::plan(future::sequential)
  } else {
    future::plan(future::multisession, workers = n_jobs)
  }


  progressr::with_progress(
    {
      p <- progressr::progressor(steps = 2 * length(shapelet_sizes))

      result <- foreach(i = seq_along(shapelet_sizes), .options.future = list(seed = TRUE)) %dofuture% {
        p("starting", amount = 0)
        p("self_mp", amount = 0)
        self_mp <- mpx(
          data = true_alarms_val,
          window_size = shapelet_sizes[i],
          exclusion_zone = 0.5,
          distance = "euclidean",
          progress = progress
          # idxs = FALSE
        )

        self_mpi <- self_mp$profile_index
        self_mp <- self_mp$matrix_profile

        if (!all(is.finite(self_mp))) {
          cli::cli_warn("self_mp contains non finite values.")
        }

        # clip values above sqrt(2 * shapelet_sizes) as they are anti-correlated
        clip <- sqrt(2 * shapelet_sizes[i])
        self_mp[self_mp > clip] <- clip

        # pad to make future comparisons between matrix profiles
        pad <- length(true_alarms_val) - length(self_mp) + 1
        self_mp_padded <- c(self_mp, rep(NA, pad))

        platos <- matrix(NA, nrow = shapelet_sizes[i], ncol = num_shapelets)
        platos_twin <- matrix(NA, nrow = shapelet_sizes[i], ncol = num_shapelets)
        plato_indices <- rep(NA, num_shapelets)
        plato_twin_indices <- rep(NA, num_shapelets)
        plato_primary_contrast <- rep(NA, num_shapelets)
        plato_nary_contrast <- rep(NA, num_shapelets)
        cps <- matrix(NA, nrow = length(self_mp_padded), ncol = num_shapelets)
        primary_cp <- NULL # rep(NA, length(self_mp_padded))

        join_mp_history <- rep(clip, length(self_mp_padded))
        past_plato <- false_alarms_val

        p("join_mp")
        for (ki in seq_len(num_shapelets)) {
          # cli::cli_alert_info("Computing plato {ki} of {num_shapelets} for size {i}({shapelet_sizes[i]}) of {length(shapelet_sizes)}.")
          #  Matrix profile AB-join between true_alarms_val and false_alarms_val
          join_mp <- mpx(
            data = true_alarms_val,
            window_size = shapelet_sizes[i],
            query = past_plato,
            exclusion_zone = 0.5,
            distance = "euclidean",
            progress = progress,
            idxs = FALSE
          )$matrix_profile

          p(paste("shapelet", ki), amount = 0)

          #  Euclidean distance values above sqrt(2*m) are equivalent to
          #    anti-correlated values
          if (!all(is.finite(join_mp))) {
            cli::cli_warn("join_mp contains non finite values.")
          }

          join_mp[join_mp > clip] <- clip

          #  pad with NA to make future comparisons between matrix profiles
          pad <- length(true_alarms_val) - length(join_mp) + 1
          join_mp <- c(join_mp, rep(NA, pad))
          na_idxs <- is.na(join_mp)

          self_mp_padded[na_idxs] <- NA
          join_mp_history <- pmin(join_mp_history, join_mp)
          #  Contrast Profile
          cp <- join_mp_history - self_mp_padded

          #  Normalize values to the range [0,1]
          cp <- cp / clip

          # discard negative values. These occur when a subsequence in T+ has a
          # closer nearest neighbor in T-. It's not a behavior of interest here
          cp <- pmax(0, cp)

          cps[, ki] <- cp

          if (ki == 1) {
            primary_cp <- cp
          }

          #  plato is the subsequence in true_alarms_val corresponding to index with
          #    largest contrast profile value
          plato_index <- which.max(cp)
          max_contrast_value <- cp[plato_index]
          plato <- true_alarms_val[plato_index:(plato_index + shapelet_sizes[i] - 1)]
          plato_indices[ki] <- plato_index
          # Reflects the contrast without appending previous platos to negativeTS
          plato_primary_contrast[ki] <- primary_cp[plato_index]
          # This may be helpful in identifying diminishing returns/redundant platos
          # if primary > nary, probably is a redundant plato
          plato_nary_contrast[ki] <- max_contrast_value
          platos[, ki] <- plato
          #  setup for next iteration
          exclusion_length <- shapelet_sizes[i]
          start_index <- max(1, (plato_index - exclusion_length))
          end_index <- min(length(true_alarms_val), ceiling(plato_index + shapelet_sizes[i] - 1 + exclusion_length))

          plato_twin_indices[ki] <- self_mpi[plato_index]
          platos_twin[, ki] <- true_alarms_val[plato_twin_indices[ki]:(plato_twin_indices[ki] + shapelet_sizes[i] - 1)]

          past_plato <- true_alarms_val[start_index:end_index]
        }
        temp <- list()
        temp[[as.character(shapelet_sizes[i])]] <- list(
          cps = cps, platos = platos, plato_indices = plato_indices,
          platos_twin = platos_twin, plato_twin_indices = plato_twin_indices,
          plato_primary_contrast = plato_primary_contrast, plato_nary_contrast = plato_nary_contrast
        )
        p("finish")
        temp
      }
    },
    handlers = progressr::handler_progress(format = ":spin [:bar] :percent :eta :message"),
    enable = progress
  )

  result <- purrr::list_flatten(result)

  return(list(pan = result, positive = true_alarms, negative = false_alarms))
}



# pan_contrast <- function(data_pos_neg, signal = "II", shapelet_sizes) {
#   checkmate::qassert(data_pos_neg, "L1")
#   checkmate::qassert(signal, "S1")
#   checkmate::qassert(shapelet_sizes, "N+")

#   "!DEBUG Compute Pan CP"

#   cli::cli_h1("Processing signal {signal}")

#   # which classes are present in the dataset?
#   classes <- unique(names(data_pos_neg[[signal]]))

#   class_result <- list()

#   # do the thing for each class
#   for (cl in classes) {
#     cli::cli_h2("Starting class {cl}")
#     profiles <- list()
#     for (i in seq_along(shapelet_sizes)) {
#       cli::cli_h2("Window size {shapelet_sizes[i]}")

#       checkmate::assert_true(data_pos_neg[[signal]][[cl]][[i]]$shapelet_size == shapelet_sizes[i])

#       reference <- data_pos_neg[[signal]][[cl]][[i]]$neg_stream
#       anomalous <- data_pos_neg[[signal]][[cl]][[i]]$pos_stream

#       self_mp <- mpx(
#         data = anomalous,
#         window_size = shapelet_sizes[i],
#         exclusion_zone = 0.5,
#         distance = "euclidean",
#         progress = FALSE,
#         idxs = FALSE
#       )$matrix_profile

#       if (!all(is.finite(self_mp))) {
#         cli::cli_warn("self_mp contains non finite values.")
#       }

#       # clip values above sqrt(2 * shapelet_sizes) as they are anti-correlated
#       clip <- sqrt(2 * shapelet_sizes[i])
#       self_mp[self_mp > clip] <- clip

#       join_mp <- mpx(
#         data = anomalous,
#         window_size = shapelet_sizes[i],
#         query = reference,
#         exclusion_zone = 0.5,
#         distance = "euclidean",
#         progress = FALSE,
#         idxs = FALSE
#       )$matrix_profile

#       if (!all(is.finite(join_mp))) {
#         cli::cli_warn("join_mp contains non finite values.")
#       }

#       join_mp[join_mp > clip] <- clip

#       contrast <- join_mp - self_mp # TruFalse - True

#       # normalize between 0 and 1
#       contrast <- contrast / clip
#       contrast[contrast < 0] <- 0
#       profiles[[i]] <- contrast
#     }
#     class_result[[cl]] <- profiles
#   }

#   result <- list()
#   result[[signal]] <- class_result

#   return(result)
# }
