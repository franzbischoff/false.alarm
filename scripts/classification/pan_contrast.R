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
# solutions <- find_solutions(score, min_cov = 18, n = 10, rep = 100000, max_red = 10, n_jobs = 2); View(solutions)
# jj <- filter_best_solutions(solutions, 2)
# plt <- plot_best_candidates(jj$data[[1]], res)

# bb |> dplyr::select(2:7) |> dplyr::filter(coverage < 20) |> ggpairs(aes(alpha = 0.05), lower = list(continuous = "smooth"))

# samples vs redundancy
# coverage x redundancy
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

score_by_segment_window <- function(true_data, false_data, contrast_profiles, quantiles = c(0.1, 1 / 3), segment_size = 2800) {
  checkmate::qassert(true_data, "N+")
  checkmate::qassert(false_data, "N+")
  checkmate::qassert(contrast_profiles, "L+")

  w_sizes <- names(contrast_profiles) # retrieve the window sizes that are stored as list labels
  c_sizes <- as.numeric(w_sizes) # then convert them to numeric

  segs <- list()
  shapes <- list()
  cont <- matrix(Inf, ncol = length(w_sizes), nrow = ncol(contrast_profiles[[1]]$cps))
  thlds <- matrix(Inf, ncol = length(w_sizes), nrow = ncol(contrast_profiles[[1]]$cps))

  for (i in seq_along(c_sizes)) {
    # get all the distance profiles of all top-k platos for the true and false data
    temp <- topk_distance_profiles(true_data, contrast_profiles, c_sizes[i])
    tp <- temp$dps
    platos <- temp$platos
    temp <- topk_distance_profiles(false_data, contrast_profiles, c_sizes[i])
    fp <- temp$dps

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
    shapes[[w_sizes[i]]] <- platos
  }

  # here we compute the total number of segments that each plato could classify
  total_counts <- as.matrix(purrr::map_dfr(segs, function(x) apply(x, 1, sum))) # never used
  colnames(cont) <- w_sizes # set the column names on the overall contrast matrix
  colnames(thlds) <- w_sizes # set the column names on the overall contrast matrix

  # NOTE: cont and segs seems to complement each other
  # cont is similar to plot_topk_contrasts (contrasts values by k vs shapelet size)
  # but it is more specific, since it takes into account the segments and not just the
  # best contrast.

  list(
    contrast = cont, # cont == overall contrast of each plato (~specificity)
    coverage = segs, # segs == coverage of each plato (~sensitivity)
    platos = shapes,
    thresholds = thlds, # thlds == threshold of each plato
    cov_counts = total_counts, # sum of segs == 1. Best is sum == num_segments  # never used
    num_segments = (length(segments) - 1)
  )


  # score <- score_candidates(score)
  # return(score)
}

score_candidates <- function(score) {
  checkmate::qassert(score, "L+")
  data <- NULL

  for (i in seq_len(nrow(score$contrast))) {
    for (j in names(score$coverage)) {
      aa <- tibble::tibble_row(
        window = j,
        k = i,
        plato = list(score$platos[[j]][, i]),
        contrast = score$contrast[i, j],
        threshold = score$thresholds[i, j],
        cov_sum = sum(score$coverage[[j]][i, ]), # same as cov_counts
        cov_idxs = list(score$coverage[[j]][i, ]) # coverage reshaped
      )
      data <- dplyr::bind_rows(data, aa)
    }
  }

  # cov_con is a ratio between the coverage and the contrast
  data <- data |>
    dplyr::filter(cov_sum > 1, contrast > 0.1) |>
    dplyr::mutate(cov_con = log10(cov_sum) * (1 / -log(contrast)), .before = cov_idxs)

  return(data)
}

# window ~ threshold
# cov_con ~ cov_sum
# cov_con ~ cov_sum ~~ threshold
# samples ~~ redundancy
# samples ~~ cov_percent ~~ coverage
# c_sd ~~ c_mean
# c_mean ~ c_median
# c_mean ~ c_median ~~ c_total
# c_total ~ samples
# k_mean !~~ c_sd
# k_mean !~~ c_mean
# cov_mean ! ~~ c_total
# cov_mean ~ cov_con_mean

filter_best_solutions <- function(
    solutions, n_sols = 1,
    c_median = c(0.1, 0.9), c_sd = c(0.1, 0.9),
    redundancy = c(0, 10), samples = c(1, 10)) {
  best <- solutions |>
    dplyr::filter(coverage == max(coverage, na.rm = TRUE)) |>
    dplyr::filter(c_median > max(c_median, na.rm = TRUE) / 2) |>
    dplyr::filter(c_sd > max(c_sd, na.rm = TRUE) / 2) |>
    dplyr::filter(redundancy == min(redundancy, na.rm = TRUE)) |>
    dplyr::arrange(c_sd) |>
    dplyr::filter(samples == min(samples, na.rm = TRUE)) |>
    dplyr::slice_head(n = n_sols)

  return(best)
}


# n = max number of shapelets to use
# rep = number of sampling repetitions
# max_red = maximum redundancy allowed
# min_cov = minimum coverage allowed

# Beam search
#| graph TD;
#|  A[Define the problem] --> B[Define the scoring function];
#|  B --> C[Initialize the set of candidate solutions];
#|  C --> D[Generate candidate solutions];
#|  D --> E[Score candidate solutions];
#|  E --> F[Select top-scoring candidate solutions];
#|  F --> G[Check for termination];
#|  G -->|Yes| H[Return the top-scoring solution];
#|  G -->|No| D;


# find_solutions
# seek high cov_percent(**) and lower red_percent for low FN
# seek for: lower red_percent(*), and samples(**) for low FP

find_solutions <- function(score, max_shapelets = 5, rep = 2000, max_red = 5, min_cov = 17, max_k = 10, n_jobs = 1) {
  segments <- score$num_segments

  if (min_cov > segments) {
    min_cov <- segments
  }

  # Convert the score object into a data frame. Also limits the topk to max_k
  score <- score_candidates(score)
  score <- score |> dplyr::filter(k <= max_k)

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

        # sample the data
        samples <- round(runif(1, 1, max_shapelets))

        ## Generate candidate solutions ## ---------------------------------------
        some_rows <- dplyr::slice_sample(score, n = samples)
        # get the size of the shapelets from the cov_idxs first row
        # must match the segments variable
        n_cols <- length(some_rows$cov_idxs[[1]])
        checkmate::assert_true(n_cols == segments)
        # reshape the cov_idxs into a matrix
        sample_data <- matrix(unlist(some_rows$cov_idxs), ncol = n_cols, byrow = TRUE)

        # computes the "pre" coverage using a colwise sum. If the column sum is zero, the
        # selected shapelet is not detecting that segment of the data
        data_coverage <- apply(sample_data, 2, sum)

        # computes the redundancy (rowwise) checking if the column sum is greater than 1.
        # This means that the segment is detected by more than one shapelet
        redundancy <- sum(data_coverage > 1, na.rm = TRUE)
        # early exit if the redundancy is too high
        if (redundancy > max_red) {
          return(NULL)
        }

        # computes the coverage (rowwise) checking if the column sum is greater than 0.
        coverage <- sum(data_coverage > 0, na.rm = TRUE)
        # early exit if the coverage is too low
        if (coverage < min_cov) {
          return(NULL)
        }

        cov_percent <- coverage / segments

        if (cov_percent < 0.5) {
          return(NULL)
        }

        keep_rows <- list()
        keep <- NULL

        ## Prune solutions efficiently ## ---------------------------------------
        # We try to reduce redudancy only if more than two shapelets were sampled
        nsamples <- nrow(sample_data)
        if (nsamples > 2) {
          # test for rows of samples that don't zero the column sum and may be dropped
          cc <- data_coverage # this is just to make the code more readable
          for (j in seq((nsamples - 1), 2)) { # iterates backwards from (nsamples - 1) to 2
            # creates a matrix with all combinations of the sequence of samples, taken j by j.
            # we start by ommiting one index, than two, than three, etc.
            comb <- combn(seq_len(nsamples), j)

            # the algorithm is pretty simple: for each combination of samples, check if the ommited
            # indexes are zeroing the column sum. If it is not, then we can drop that indexes.
            for (k in seq_len(ncol(comb))) { # iterates over the combination matrix, columnwise
              rr <- sample_data[comb[, k], , drop = FALSE]
              rr <- apply(rr, 2, sum) # here we compute the new "pre" coverage

              # (rr & cc) is an efficient way to check if both arrays are > 0 columnwise
              dd <- rr & cc
              # then, we compare the column sums of the new "pre" coverage with the original
              # if any of the columns have the same sum, we keep searching
              # if not, we store the ommited indexes, since it is a combination that cannot be dropped
              if (!any(rr[dd] == cc[dd])) {
                keep <- setdiff(seq_len(nsamples), comb[, k])
                keep_rows <- c(keep_rows, list(keep))
              }
            }

            # if we found a combination that cannot be dropped, we can stop searching
            # this combination is already the smallest combination that reduces redundancy and keeps coverage
            # we can have more than one combination in this loop. Later, we will select the more appropriate
            if (length(keep_rows) > 0) {
              break
            }
          }
        }

        if (length(keep_rows) == 1) {
          # if we found only one combination that reduces redundancy, we keep only that combination
          some_rows <- some_rows[keep_rows[[1]], ]
          samples <- length(keep_rows[[1]])
        } else if (length(keep_rows) > 1) {
          # if we found more than one combination that reduces redundancy, we keep the combination
          # that maximizes the score function
          new_rows <- NULL
          for (j in seq_len(length(keep_rows))) {
            temp <- dplyr::bind_cols(idx = j, some_rows[keep_rows[[j]], ])
            new_rows <- dplyr::bind_rows(new_rows, temp)
          }

          new_rows <- new_rows |>
            dplyr::group_by(idx) |>
            dplyr::summarise(
              k_mean = mean(k, na.rm = TRUE), c_total = sum(contrast, na.rm = TRUE),
              c_mean = mean(contrast, na.rm = TRUE)
            )
          new_rows <- new_rows |>
            dplyr::arrange(dplyr::desc(k_mean), dplyr::desc(c_total), c_mean) |>
            dplyr::slice_head(n = 1)
          new_idx <- new_rows |> dplyr::pull(idx)
          some_rows <- some_rows[keep_rows[[new_idx]], ]

          samples <- length(keep_rows[[new_idx]])
        }

        # else, keep the original sampled rows

        data_coverage <- apply(matrix(unlist(some_rows$cov_idxs), ncol = segments, byrow = TRUE), 2, sum)
        redundancy <- sum(data_coverage > 1, na.rm = TRUE)
        coverage <- sum(data_coverage > 0, na.rm = TRUE)
        # normalize the coverage and redundancy by the number of segments to allow comparison
        # between different folds.
        cov_percent <- coverage / segments
        red_percent <- redundancy / segments


        list(
          c_total = sum(some_rows$contrast, na.rm = TRUE),
          c_median = median(some_rows$contrast, na.rm = TRUE),
          c_mean = mean(some_rows$contrast, na.rm = TRUE),
          c_sd = ifelse(length(some_rows$contrast) > 1, sd(some_rows$contrast, na.rm = TRUE), 0),
          cov_con_mean = mean(some_rows$cov_con, na.rm = TRUE),
          k_mean = mean(some_rows$k, na.rm = TRUE),
          cov_mean = mean(some_rows$cov_sum, na.rm = TRUE),
          coverage = coverage,
          cov_percent = cov_percent,
          redundancy = redundancy,
          red_percent = red_percent,
          samples = samples, cand = some_rows
        )
      }, .options = furrr::furrr_options(seed = 2023, scheduling = 1))
    },
    delay_terminal = TRUE,
    handlers = progressr::handler_progress(format = "[:bar] :percent :eta :message", interval = 10) # intrusiveness = 100,
  )

  sols <- list_dfr(sols)

  if (length(sols) == 0) {
    cli::cli_warn("No solution found. Try decreasing coverage value.")
  } else {
    sols <- tidyr::unnest(sols, cols = cand) |>
      dplyr::distinct_all() |>
      tidyr::nest(data = c(window, k, contrast, threshold, plato, cov_sum, cov_con, cov_idxs))
    cli::cli_alert_info("done.")
  }

  return(sols)
}


# The scoring function will try to minimize at first one of the "min" arguments
# minimizing fp will try to minimize the number of false positives
# minimizing fn will try to minimize the number of false negatives
# For minimizing FP, these variables have the following correlation/importance:
#       - samples: 0.492/1.07
#       - red_percent: 0.192/0.490
#       - c_total: 0.470/0.247
#       - cov_percent: 0.260/0.173
# For minimizing FN, these variables have the following correlation/importance:
#       - cov_percent: -0.854 / 7.56  *** score
#       - c_total: -0.244 / 2.64
#       - samples: -0.346 / 2.26
#       - red_percent: -0.402 / 1.79
#       - k_mean: -0.143 / 0.524

score_solutions <- function(object, min = c("fp", "fn")) {
  checkmate::qassert(object, "L+")
  checkmate::qassert(min, "S")

  min <- match.arg(min)

  if (min == "fp") { # hard
    # minimize false positives
    object <- object |>
      dplyr::mutate( # 0.493
        score = c_total * 0.11609 + cov_percent * 0.04498 + red_percent * 0.09408 + samples * 0.52644
      )
  } else if (min == "fn") {
    # minimize false negatives
    object <- object |>
      dplyr::mutate( # 0.5
        score = c_total * -0.64416 + cov_percent * -6.45624 + red_percent * -0.71958 + k_mean * -0.074932 + samples * -0.78196
      )
  }
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

  w_sizes <- names(contrast_profiles) # retrieve the window sizes that are stored as list labels
  w_size <- as.character(window_size) # convert the numeric window_size to character

  if (!(w_size %in% w_sizes)) {
    cli::cli_abort("Invalid window_size.")
  }

  platos <- contrast_profiles[[w_size]]$platos # all top-k platos
  platos_twin <- contrast_profiles[[w_size]]$platos_twin # and the corresponding twins

  dps <- matrix(NA, ncol = ncol(platos), nrow = length(data) - window_size + 1)
  queries <- (platos + platos_twin) / 2 # average of the plato and its twin

  # iterate over all top-k platos
  for (i in seq_len(ncol(platos))) {
    query <- queries[, i]

    dp <- dist_profile(data, query) # compute the distance profile of the plato against the data
    dps[, i] <- dp # store the distance profile
  }

  return(list(dps = dps, platos = queries))
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
  suppressMessages(require(doFuture))

  "!DEBUG Compute Pan CP"

  idxs <- (split$data$alarm == "true")

  true_alarms <- NULL
  false_alarms <- NULL

  split_size <- length(split$data$values[[1]]) + 300

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
          cli::cli_warn("self_mp contains non finite values. This may happen for small windows.")
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
            cli::cli_warn("join_mp contains non finite values. This may happen for small windows.")
          }

          join_mp[join_mp > clip] <- clip

          #  pad with NA to make future comparisons between matrix profiles
          pad <- length(true_alarms_val) - length(join_mp) + 1
          join_mp <- c(join_mp, rep(NA, pad))
          na_idxs <- is.na(join_mp)

          self_mp_padded[na_idxs] <- NA
          join_mp_history <- pmin(join_mp_history, join_mp, na.rm = TRUE)
          #  Contrast Profile
          cp <- join_mp_history - self_mp_padded

          #  Normalize values to the range [0,1]
          cp <- cp / clip

          # discard negative values. These occur when a subsequence in T+ has a
          # closer nearest neighbor in T-. It's not a behavior of interest here
          cp <- pmax(0, cp, na.rm = TRUE)

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
          start_index <- max(1, (plato_index - exclusion_length), na.rm = TRUE)
          end_index <- min(length(true_alarms_val), ceiling(plato_index + shapelet_sizes[i] - 1 + exclusion_length), na.rm = TRUE)

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

  return(list(pan = result, positive = true_alarms, negative = false_alarms, split_size = split_size))
}

compute_metrics_topk <- function(fold, shapelets, n_jobs = 1, progress = FALSE) {
  checkmate::qassert(fold, "L+")
  checkmate::qassert(shapelets, "d+")
  require(doFuture)

  "!DEBUG Compute Metrics"

  if (n_jobs == 1) {
    future::plan(future::sequential)
  } else {
    future::plan(future::multisession, workers = n_jobs)
  }

  # prepare data
  data <- list()
  for (k in seq_along(fold$data$values)) {
    alarm <- (fold$data$alarm[[k]] == "true")

    if (sum(fold$data$values[[k]][1:1000], na.rm = TRUE) == 0) { # skip if no data
      next
    }

    fold$data$values[[k]] <- vctrs::vec_fill_missing(fold$data$values[[k]])
    vdata <- validate_data(fold$data$values[[k]], 20)
    if (anyNA(vdata)) {
      cli::cli_warn("NA values in fold data {k}.")
    }

    data[[k]] <- list(data = vdata, alarm = alarm)
  }

  progressr::with_progress(
    {
      p <- progressr::progressor(steps = nrow(shapelets))

      res <- foreach(i = seq_len(nrow(shapelets)), .options.future = list(seed = TRUE)) %dofuture% {
        shapelet <- shapelets[i, ]

        tp <- 0
        fp <- 0
        tn <- 0
        fn <- 0

        thresholds <- shapelet$data[[1]]$threshold
        windows <- shapelet$data[[1]]$window
        platos <- shapelet$data[[1]]$plato

        for (k in seq_along(data)) {
          # p(paste("data", k), amount = 0)
          if (is.null(data[[k]])) {
            next
          }

          class <- NULL

          for (j in seq_along(windows)) {
            dp <- dist_profile(data[[k]]$data, platos[[j]])

            class <- c(class, (min(dp, na.rm = TRUE) < thresholds[j]))
          }

          class <- any(class) # ((sum(class) / length(windows)) > 0.2)

          if (class == data[[k]]$alarm) {
            # hit
            if (isTRUE(class)) {
              # true positive
              tp <- tp + 1
            } else {
              # true negative
              tn <- tn + 1
            }
          } else {
            # miss
            if (isTRUE(class)) {
              # false positive
              fp <- fp + 1
            } else {
              # false negative
              fn <- fn + 1
            }
          }
        }

        precision <- tp / (tp + fp)
        recall <- tp / (tp + fn)
        specificity <- tn / (tn + fp)
        FOR <- fn / (fn + tn)
        accuracy <- (tp + tn) / (tp + tn + fp + fn)
        f1 <- 2 * tp / (2 * tp + fp + fn)
        p4 <- (4 * tp * tn) / (4 * tp * tn + (tp + tn) * (fp + fn))
        mcc <- (tp * tn - fp * fn) / sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
        majority <- max(tp + fn, fp + tn) / (tp + fp + tn + fn)
        km <- (accuracy - majority) / (1 - majority)
        kappa <- 2 * (tp * tn - fp * fn) / ((tp + fn) * (fn + tn) + (tp + fp) * (fp + tn))

        p(paste("finish", i))

        list(
          tp = tp, fp = fp, tn = tn, fn = fn,
          precision = precision, recall = recall,
          specificity = specificity, FOR = FOR,
          accuracy = accuracy, f1 = f1,
          p4 = p4, mcc = mcc, km = km, kappa = kappa
        )
      }
    },
    handlers = progressr::handler_progress(format = ":spin [:bar] :percent :eta :message"),
    enable = progress
  )

  return(res)
}

rrank <- function(x, n, r) {
  y <- sort(unique(round(abs(x), n))) + (1 / 10^n)
  y[r]
}

# aaa <- dplyr::bind_rows(list_dfr(test_classifiers_self[[1]][[1]]),
# list_dfr(test_classifiers_self[[1]][[2]]),
# list_dfr(test_classifiers_self[[1]][[3]]),
# list_dfr(test_classifiers_self[[1]][[4]]),
# list_dfr(test_classifiers_self[[1]][[5]]))
# GGally::ggpairs(aaa[, 5:13])
# aaa <- aaa |> dplyr::mutate(fnr = fn / (tp + fn), fpr = fp / (fp + tn))

compute_overall_metric <- function(all_folds) {
  tp <- fp <- tn <- fn <- acc <- ff <- 0

  full_size <- 0

  for (fold in all_folds) {
    full_size <- full_size + nrow(fold)

    for (i in seq_len(nrow(fold))) {
      shape <- fold[i, ]
      tp <- tp + shape$tp
      fp <- fp + shape$fp
      tn <- tn + shape$tn
      fn <- fn + shape$fn
      ff <- ff + shape$f1
    }
  }

  tm <- (2 * tp) / (2 * tp + fp + fn)
  fm <- (2 * tn) / (2 * tn + fp + tp)
  f1_micro <- (tm + fm) / 2
  f1_macro <- (ff / full_size)
  f1_weighted <- ((tp + fp) * tm + (fn + tn) * fm) / (tp + tn + fp + fn)
  pre <- tp / (tp + fp)
  rec <- tp / (tp + fn)
  spec <- tn / (tn + fp)
  acc <- (tp + tn) / (tp + tn + fp + fn)
  p4 <- (4 * tp * tn) / (4 * tp * tn + (tp + tn) * (fp + fn))
  mcc <- (tp * tn - fp * fn) / sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  majority <- max(tp + fn, fp + tn) / (tp + fp + tn + fn)
  km <- (acc - majority) / (1 - majority)
  kappa <- 2 * (tp * tn - fp * fn) / ((tp + fn) * (fn + tn) + (tp + fp) * (fp + tn))

  return(tibble::tibble(
    tp = tp, fp = fp, tn = tn, fn = fn,
    precision = pre, recall = rec, specificity = spec,
    accuracy = acc, f1_micro = f1_micro, f1_macro = f1_macro, f1_weighted = f1_weighted,
    p4 = p4, mcc = mcc, km = km, kappa = kappa
  ))
}

combine_metrics <- function(metrics, shapelets) {
  aa <- tibble::as_tibble(purrr::transpose(metrics))
  aa <- dplyr::mutate_all(aa, as.numeric)
  aa <- dplyr::bind_cols(shapelets, aa) |>
    # dplyr::select(-data) |> ####### The final `model` we need is the shapelet
    dplyr::mutate(across(!where(is.list), as.numeric))

  sup_spec <- quantile(aa$specificity, 0.75, na.rm = TRUE)
  sup_prec <- quantile(aa$precision, 0.75, na.rm = TRUE)
  min_fp <- min(aa$fp, na.rm = TRUE)
  min_fn <- min(aa$fn, na.rm = TRUE)

  aa <- aa |>
    dplyr::filter(
      p4 >= 0.1
    ) |>
    tidyr::drop_na() |>
    dplyr::arrange(fp, fn)
  # dplyr::filter(
  #   precision >= sup_prec,
  #   specificity >= sup_spec
  # ) |>
  # dplyr::arrange(fp, fn)
  # dplyr::slice_head(n = 50)

  return(aa)
}



# tp <- fp <- tn <- fn <- acc <- ff <- 0
# for (i in 1:5) {
#   tp <- tp + test_classifiers_self[[1]][[i]]$tp
#   fp <- fp + test_classifiers_self[[1]][[i]]$fp
#   tn <- tn + test_classifiers_self[[1]][[i]]$tn
#   fn <- fn + test_classifiers_self[[1]][[i]]$fn
#   acc <- acc + test_classifiers_self[[1]][[i]]$accuracy
#   ff <- ff + test_classifiers_self[[1]][[i]]$f1
# }
# tm <- (2 * tp) / (2 * tp + fp + fn)
# fm <- (2 * tn) / (2 * tn + fp + tp)
# w <- ((tp + fp) * tm + (fn + tn) * fm) / (tp + tn + fp + fn)
# p4 <- (4 * tp * tn) / (4 * tp * tn + (tp + tn) * (fp + fn))

# # markedness
# mk <- (tp / (tp + fp)) + (tn / (tn + fn)) - 1
# # matthews correlation coefficient
# mcc <- (tp * tn - fp * fn) / sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
# # kappa
# kappa <- 2 * (tp * tn - fp * fn) / ((tp + fn) * (fn + tn) + (tp + fp) * (fp + tn))

# print(tm) # micro true
# print(fm) # micro false
# print(w) # weigthed
# print((tm + fm) / 2) # micro
# print(ff / 5) # macro




# micro-averaged f1 score






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
