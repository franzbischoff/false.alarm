# pos <- R.matlab::readMat(here::here("dev/matlab/pos.mat"))
# pos <- as.numeric(pos[[1]][1,])
# neg <- R.matlab::readMat(here::here("dev/matlab/neg.mat"))
# neg <- as.numeric(neg[[1]][1,])
# w <- 91
# k <- 10
# res <- rf_contrast_profile(pos, neg, w, k)
## max_indices <- max.col(t(res$rfcp))
# max_values <- apply(res$rfcp, 2, max)
# best_k <- which.max(max_values)
# highest_contrast_value <- max_values[best_k]

# function [plato, rfcp, rfmp_aa_idxs] = rf_contrast_profile(data_pos, data_neg, window_size, max_k, forcePlot)
rf_contrast_profile <- function(data_pos, data_neg, window_size, max_k) {
  ### Input:
  ###   data_pos: A time series containing at least two instances of a desired behavior
  ###   data_neg: A time series containing zero instances of a desired behavior
  ###   window_size: The approximate window size of the desired behavior
  ###   max_k: Number of nearest neighbors to search. Does not affect
  ###     time complexity
  ###
  ### Output:
  ###   plato: The subsequence that most distinguishes
  ###          data_pos from data_neg. Determined based on
  ###          maximum of root mean squared contrast for each time index.
  ###   rfcp: Contrast Profile, which indicates subsequence within
  ###       data_pos that are less conserved in data_neg
  ###   rfmp_aa_idxs: k_max nearest neighbor indices within data_pos

  initial_pos_subcount <- length(data_pos) - window_size + 1
  initial_neg_subcount <- length(data_neg) - window_size + 1

  if (!(is.finite(window_size) && floor(window_size) == window_size) ||
    (window_size < 2) || (initial_pos_subcount < 2) || (initial_neg_subcount < 2)) {
    cli::cli_abort("subsequence length must be an integer value between 2 and the length of the timeseries")
  }

  # Matrix profile self-join using data_pos
  rfmp_aa <- rf_matrix_profile(data_pos, data_pos, window_size, max_k)

  ### Euclidean distance values above sqrt(2*window_size) are equivalent to
  ###   anti-correlated values
  clip <- sqrt(2 * window_size)
  rfmp_aa$matrix_profile[rfmp_aa$matrix_profile > clip] <- clip
  ### pad with NaN to make future comparisons between matrix profiles
  #     padLength = length(data_pos) - length(MP_AA) + 1;
  #     MP_AA = [MP_AA;NaN(padLength,1)];

  ### Matrix profile AB-join between data_pos and data_neg
  rfmp_ab <- rf_matrix_profile(data_pos, data_neg, window_size, max_k)

  ### Euclidean distance values above sqrt(2*window_size) are equivalent to
  ###   anti-correlated values
  rfmp_ab$matrix_profile[rfmp_ab$matrix_profile > clip] <- clip
  ### pad with NaN to make future comparisons between matrix profiles
  #     padLength = length(data_pos) - length(MP_AB) + 1;
  #     MP_AB = [MP_AB;NaN(padLength,1)];

  #### If a non-overlapping candidates is less than max_k
  effective_max_k <- min(nrow(rfmp_aa$matrix_profile), nrow(rfmp_ab$matrix_profile))
  if (max_k > effective_max_k) {
    cli::cli_warn("Choice of max_k={max_k}, was too large for dataset, setting to {effective_max_k}")
    rfmp_aa$matrix_profile <- rfmp_aa$matrix_profile[1:effective_max_k, ]
    rfmp_ab$matrix_profile <- rfmp_ab$matrix_profile[1:effective_max_k, ]
    max_k <- effective_max_k
  }

  ### Contrast Profile
  rfcp <- rfmp_ab$matrix_profile - rfmp_aa$matrix_profile
  ### Normalize values to the range [0,1]
  rfcp <- rfcp / clip
  rfcp[rfcp < 0] <- 0

  ### plato is the subsequence in data_pos corresponding to index with
  ###   largest contrast profile value
  #     [maxContrastValues, platoIndices] = max(rfcp,[],2);
  #     [maxContrastValue, KBest] = max(maxContrastValues);
  #     plato_idx = platoIndices(KBest); v0.1385601
  #     plato = tsPos(plato_idx:plato_idx+window_size-1);

  ### Alternative Plato calculation
  ### Maximum root mean squared of contrast values
  cp_mean <- vector(mode = "numeric", length = ncol(rfcp))

  for (ti in seq_len(ncol(rfcp) - window_size + 1)) {
    subsequence <- data_pos[ti:(ti + window_size - 1)]

    if (sum(is.na(subsequence)) > 0) {
      next
    }

    cp_mean[ti] <- sqrt(sum(rfcp[, ti]^2)) / sqrt(max_k) # root mean squared
  }

  max_cp_index <- which.max(cp_mean)
  max_contrast <- cp_mean[max_cp_index]
  plato_idx <- max_cp_index
  start_idx <- max_cp_index
  end_idx <- start_idx + window_size - 1
  plato <- data_pos[start_idx:end_idx]

  ### plato_twin is the nearest neighbor of plato within positieTS
  ###   It is not necessarily the second largest contrast profile value
  plato_twin_idx <- rfmp_aa$profile_idxs[1, plato_idx]

  if (plato_twin_idx + window_size - 1 > length(data_pos)) {
    plato_twin_idx <- length(data_pos) - window_size + 1
  }

  return(list(plato = plato, rfcp = rfcp, rfmp_aa_idxs = rfmp_aa$profile_idxs))
}

# function [rfmp, rfmp_idxs] = rf_matrix_profile(ts_a, ts_b, window_size, max_k)
rf_matrix_profile <- function(ts_a, ts_b, window_size, max_k) {
  self_flag <- 0

  if (length(ts_a) == length(ts_b) && mean(ts_a == ts_b) > 0.9) {
    self_flag <- 1
    cli::cli_warn("Self-join detected, setting self_flag to 1")
  } else {
    cli::cli_warn("Self-join not detected, setting self_flag to 0")
  }

  ts_a_no_nan <- ts_a
  ts_a_no_nan[is.na(ts_a)] <- mean(ts_a, na.rm = TRUE)

  ts_b_no_nan <- ts_b
  ts_b_no_nan[is.na(ts_b)] <- mean(ts_b, na.rm = TRUE)

  rfmp <- matrix(NA, nrow = max_k, ncol = length(ts_a_no_nan))
  rfmp_idxs <- matrix(NA, nrow = max_k, ncol = length(ts_a_no_nan))

  for (index_time in 1:(length(ts_a_no_nan) - window_size + 1)) {
    subsequence <- ts_a[index_time:(index_time + window_size - 1)]

    if (sum(is.na(subsequence)) > 0) {
      next
    }

    dp <- dist_profile(ts_b_no_nan, subsequence) #  / sqrt(window_size) # why original code does this?

    for (nan_idx in 1:(length(ts_b) - window_size + 1)) {
      if (is.na(ts_b[nan_idx])) {
        start_idx <- max(1, nan_idx - window_size + 1)
        dp[start_idx:nan_idx] <- NA
      }
    }

    ### TODO: get candidates so exclusion zones are used
    sorted_idxs <- nn_selection(dp, window_size, max_k + self_flag)
    sorted_dist <- sorted_idxs$sorted_dist
    sorted_idxs <- sorted_idxs$sorted_idxs
    k_min <- min(max_k + self_flag, length(sorted_idxs))
    rfmp[1:(k_min - self_flag), index_time] <- sorted_dist[(1 + self_flag):k_min]
    rfmp_idxs[1:(k_min - self_flag), index_time] <- sorted_idxs[(1 + self_flag):k_min]
  }

  return(list(matrix_profile = rfmp, profile_idxs = rfmp_idxs))
}

# endTime = toc;
# fprintf("MP Frequency completed in #.2f seconds\n",endTime);

# visualizeMMPAB(ts_a_no_nan, PMPKNN, PMPiKNN, [], [], [], 100, 1:100, "KNN","");

### Now output the best candidate for each number of nearest neighbors
# [bestCandidateDistances, minIndices] = min(PMPKNN, [],2);
# plotSortedBinnedSamples(minIndices, ts_a_no_nan, [], window_size, 10,10,"");

nn_selection <- function(dp, window_size, max_k = NULL) {
  # Returns the indices of the lowest values
  #   sorted by lowest values
  #   using excluson zone around matches
  #   k_max (optional): return Top-K min indices. Default return all possible indices,
  #   WARNING: may not

  exclusion_length <- window_size
  k_max <- 2 * floor(length(dp) / (window_size))

  if (is.null(max_k)) {
    max_k <- k_max
  }

  if (max_k > k_max) {
    cli::cli_warn("max_k is greater than the maximum possible value of {k_max}. Setting max_k to {k_max}.")
    max_k <- k_max
  }

  sorted_dp <- sort(dp, index.return = TRUE)
  sorted_dp_idxs <- sorted_dp$ix

  exclusion_zone <- rep(0, length(dp))

  sorted_lowest_indices <- rep(NA, max_k)
  corresponding_distances <- rep(NA, max_k)
  index <- 1
  iter_kum <- 1 # for debugging

  while (index <= max_k && iter_kum < length(sorted_dp_idxs)) {
    trial_idx <- sorted_dp_idxs[iter_kum]

    if (exclusion_zone[trial_idx] == 0 && !is.na(dp[trial_idx])) {
      sorted_lowest_indices[index] <- trial_idx
      corresponding_distances[index] <- dp[trial_idx]
      index <- index + 1
      start_exclusion <- max(1, trial_idx - exclusion_length)
      end_exclusion <- min(length(dp), trial_idx + exclusion_length)
      exclusion_zone[start_exclusion:end_exclusion] <- 1
    }

    iter_kum <- iter_kum + 1
  }

  sorted_lowest_indices <- sorted_lowest_indices[!is.na(sorted_lowest_indices)]
  corresponding_distances <- corresponding_distances[!is.na(sorted_lowest_indices)]

  return(list(sorted_idxs = sorted_lowest_indices, sorted_dist = corresponding_distances))
}
