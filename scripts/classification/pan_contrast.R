
pan_contrast <- function(data_pos_neg, signal = "II", shapelet_sizes) {
  checkmate::qassert(data_pos_neg, "L1")
  checkmate::qassert(signal, "S1")
  checkmate::qassert(shapelet_sizes, "N+")

  "!DEBUG Compute Pan CP"

  cli::cli_h1("Processing signal {signal}")

  # which classes are present in the dataset?
  classes <- unique(names(data_pos_neg[[signal]]))

  class_result <- list()

  # do the thing for each class
  for (cl in classes) {
    cli::cli_h2("Starting class {cl}")
    profiles <- list()
    for (i in seq_along(shapelet_sizes)) {
      cli::cli_h2("Window size {shapelet_sizes[i]}")

      checkmate::assert_true(data_pos_neg[[signal]][[cl]][[i]]$shapelet_size == shapelet_sizes[i])

      reference <- data_pos_neg[[signal]][[cl]][[i]]$neg_stream
      anomalous <- data_pos_neg[[signal]][[cl]][[i]]$pos_stream

      self_mp <- mpx(
        data = anomalous,
        window_size = shapelet_sizes[i],
        exclusion_zone = 0.5,
        distance = "euclidean",
        progress = FALSE,
        idxs = FALSE
      )$matrix_profile

      if (any(!is.finite(self_mp))) {
        cli::cli_warn("self_mp contains non finite values.")
      }

      # clip values above sqrt(2 * shapelet_sizes) as they are anti-correlated
      clip <- sqrt(2 * shapelet_sizes[i])
      self_mp[self_mp > clip] <- clip

      join_mp <- mpx(
        data = anomalous,
        window_size = shapelet_sizes[i],
        query = reference,
        exclusion_zone = 0.5,
        distance = "euclidean",
        progress = FALSE,
        idxs = FALSE
      )$matrix_profile

      if (any(!is.finite(join_mp))) {
        cli::cli_warn("join_mp contains non finite values.")
      }

      join_mp[join_mp > clip] <- clip

      contrast <- join_mp - self_mp

      # normalize between 0 and 1
      contrast <- contrast / clip
      contrast[contrast < 0] <- 0
      profiles[[i]] <- contrast
    }
    class_result[[cl]] <- profiles
  }

  result <- list()
  result[[signal]] <- class_result

  return(result)
}
