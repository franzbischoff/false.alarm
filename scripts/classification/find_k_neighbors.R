find_k_neighbors <- function(data_pos_neg, data_shapelets, signal = "II", n_neighbors = 3, corr_min = 0.8, exclusion_zone = NULL) {
  checkmate::qassert(data_pos_neg, "L1")
  checkmate::qassert(data_shapelets, "L1")
  checkmate::qassert(signal, "S1")
  checkmate::qassert(n_neighbors, "N1")
  checkmate::qassert(corr_min, "N1")
  checkmate::qassert(exclusion_zone, "N1")

  cli::cli_h1("Processing signal {signal}")

  # which classes are present in the dataset?
  classes <- unique(names(data_pos_neg[[signal]]))

  class_result <- list()

  # do the thing for each class
  for (cl in classes) {
    cli::cli_h2("Starting class {cl}")

    data <- data_pos_neg[[signal]][[cl]]$pos_stream
    data_len <- length(data)
    num_shapelets <- length(data_shapelets[[signal]][[cl]])

    shape <- list()
    for (k in seq_len(num_shapelets)) {
      cli::cli_inform("Starting shaplet {k}.")
      nn <- n_neighbors
      query <- data_shapelets[[signal]][[cl]][[k]]$plato
      query_len <- length(query)
      profile_len <- data_len - query_len + 1
      ez <- round(query_len * exclusion_zone + .Machine$double.eps^0.5)

      if (profile_len < (query_len + 2 * ez)) {
        cli::cli_warn("Data is too short for looking for neighbors. Stopped on shapelet {k} for class {cl}.")
        break
      }

      if (profile_len < nn * (query_len + 2 * ez)) {
        old_value <- nn
        nn <- floor(profile_len / (query_len + 2 * ez))
        cli::cli_inform("Too many neighbors asked for this query. Changing from {old_value} to {nn}.")
      }

      pre <- false.alarm::mass_pre(data, query_len, query)
      pre$data_sd[pre$data_sd <= 0] <- .Machine$double.eps^0.5 # HACK: need to fix mass3_rcpp()
      pre$query_sd[pre$query_sd <= 0] <- .Machine$double.eps^0.5 # HACK: need to fix mass3_rcpp()
      dist_profile <- false.alarm::mass(pre, data, query)$distance_profile
      dist_profile[dist_profile < 0] <- 0
      dist_profile <- sqrt(dist_profile)

      # remove self
      min_idx <- which.min(dist_profile)
      start <- max(min_idx - ez, 1)
      end <- min(min_idx + query_len - 1 + ez, profile_len)
      dist_profile[start:end] <- Inf

      st <- sort(dist_profile, index.return = TRUE)
      dist_sorted <- st$x
      dist_sorted_idx <- st$ix

      valid <- is.finite(dist_sorted)
      dist_sorted <- dist_sorted[valid]
      dist_sorted_idx <- dist_sorted_idx[valid]

      dist_max <- false.alarm::corr_ed(corr_min, query_len)

      neighbors <- list()

      for (i in seq_len(nn)) {
        idx <- dist_sorted_idx[1]
        dist <- dist_sorted[1]

        if (dist > dist_max) {
          break
        }

        neighbors[[i]] <- list(
          data = data[idx:(idx + query_len - 1)],
          dist = dist,
          idx = idx
        )
        start <- max(idx - ez, 1)
        end <- min(idx + query_len - 1 + ez, profile_len)
        keep <- !(dist_sorted_idx %in% (start:end))
        dist_sorted <- dist_sorted[keep]
        dist_sorted_idx <- dist_sorted_idx[keep]
      }
      shape[[k]] <- neighbors
    }
    class_result[[cl]] <- shape
  }
  result <- list()
  result[[signal]] <- class_result

  return(result)
}
