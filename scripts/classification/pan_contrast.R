
find_k_shapelets <- function(data_pos_neg, signal = "II", shapelet_sizes) {
  checkmate::qassert(data_pos_neg, "L1")
  checkmate::qassert(signal, "S1")
  checkmate::qassert(shapelet_sizes, "N+")

  cli::cli_h1("Processing signal {signal}")

  # which classes are present in the dataset?
  classes <- unique(names(data_pos_neg[[signal]]))

  class_result <- list()

  # do the thing for each class
  for (cl in classes) {
    cli::cli_h2("Starting class {cl}")

    reference <- data_pos_neg[[signal]][[cl]]$neg_stream
    anomalous <- data_pos_neg[[signal]][[cl]]$pos_stream

    self_mp <- mpx(
      data = anomalous,
      window_size = shapelet_size,
      exclusion_zone = 0.5,
      distance = "euclidean",
      progress = FALSE
    )$matrix_profile

    # clip values above sqrt(2 * shapelet_size) as they are anti-correlated
    clip <- sqrt(2 * shapelet_size)
    self_mp[self_mp > clip] <- clip

    if (any(!is.finite(self_mp))) {
      cli::cli_warn("self_mp contains non finite values.")
    }

    join_mp_history <- rep(sqrt(2 * shapelet_size), length(self_mp))
    con <- list()
    for (k in seq_len(num_shapelets)) {
      cli::cli_inform("Starting shapelet {k}.")

      join_mp <- mpx(
        data = anomalous,
        window_size = shapelet_size,
        query = reference,
        exclusion_zone = 0.5,
        distance = "euclidean",
        progress = FALSE
      )$matrix_profile

      # clip values above sqrt(2 * shapelet_size) as they are anti-correlated
      join_mp[join_mp > clip] <- clip

      if (any(!is.finite(join_mp))) {
        cli::cli_warn("join_mp {k} contains non finite values.")
      }

      join_mp_history <- pmin(join_mp_history, join_mp)

      contrast <- join_mp_history - self_mp

      # normalize between 0 and 1
      contrast <- contrast / clip
      contrast[contrast < 0] <- 0

      plato_idx <- which.max(contrast)
      plato_val <- contrast[plato_idx]

      if (plato_val == 0) { # there is no more plato's
        cli::cli_inform("There is no more shapelets here. Stopping on {k}.")
        break
      }

      plato <- anomalous[plato_idx:(plato_idx + shapelet_size - 1)]

      con[[k]] <- list(contrast = contrast, plato = plato, plato_idx = plato_idx, plato_val = plato_val)

      # next iteration
      excl_zone <- shapelet_size
      excl_start <- max(1, plato_idx - excl_zone)
      excl_end <- min(length(anomalous), ceiling(plato_idx + shapelet_size - 1 + excl_zone))
      reference <- anomalous[excl_start:excl_end]
    }
    class_result[[cl]] <- con
  }

  result <- list()
  result[[signal]] <- class_result

  return(result)
}
