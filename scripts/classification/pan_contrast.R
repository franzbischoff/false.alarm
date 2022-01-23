
pan_contrast <- function(data_pos_neg, signal = "II", shapelet_sizes) {
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

    platos <- list()
    for (i in shapelet_sizes) {
      self_mp <- mpx(
        data = anomalous,
        window_size = i,
        exclusion_zone = 0.5,
        distance = "euclidean",
        progress = FALSE
      )$matrix_profile

      # clip values above sqrt(2 * shapelet_sizes) as they are anti-correlated
      clip <- sqrt(2 * shapelet_sizes)
      self_mp[self_mp > clip] <- clip

      if (any(!is.finite(self_mp))) {
        cli::cli_warn("self_mp contains non finite values.")
      }

      join_mp <- mpx(
        data = anomalous,
        window_size = shapelet_sizes,
        query = reference,
        exclusion_zone = 0.5,
        distance = "euclidean",
        progress = FALSE
      )$matrix_profile

      contrast <- join_mp - self_mp

      # normalize between 0 and 1
      contrast <- contrast / clip
      contrast[contrast < 0] <- 0

      plato_idx <- which.max(contrast)
      plato_val <- contrast[plato_idx]
      plato <- anomalous[plato_idx:(plato_idx + shapelet_sizes - 1)]
    }
    platos[[i]] <- list(plato_idx, plato_val, plato)
  }
  class_result[[cl]] <- platos

  result <- list()
  result[[signal]] <- class_result

  return(result)
}
