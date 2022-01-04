
find_snippets <- function(data_pos_neg, signal = "II", reverse = FALSE) {
  checkmate::qassert(data_pos_neg, "L1")
  checkmate::qassert(signal, "S1")
  checkmate::qassert(n_workers, "N[1,6]")

  cli::cli_h1("Processing signal {signal}")

  # which classes are present in the dataset?
  classes <- unique(names(data_pos_neg[[signal]]))

  class_result <- list()

  # do the thing for each class
  for (cl in classes) {
    cli::cli_h2("Starting class {cl}")

    if (reverse) {
      reference <- data_pos_neg[[signal]][[cl]]$pos_stream
      query <- data_pos_neg[[signal]][[cl]]$neg_stream
    } else {
      reference <- data_pos_neg[[signal]][[cl]]$neg_stream
      query <- data_pos_neg[[signal]][[cl]]$pos_stream
    }

    con <- contrast(reference, query,
      window_size = data_pos_neg[[signal]][[cl]]$snippet_size,
      positive_matrix = data_pos_neg[[signal]][[cl]]$pos_mp,
      exclusion_zone = 0.5, distance = "euclidean",
      n_workers = 1, progress = FALSE
    )
    class_result[[cl]] <- con
  }

  result <- list()
  result[[signal]] <- class_result

  return(result)
}
