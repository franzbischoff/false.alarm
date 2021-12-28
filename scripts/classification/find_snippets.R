build_pos_neg <- function(initial_split, signals = "II", windows_set = 250,
                          validate = FALSE, n_workers = 1) {
  checkmate::qassert(initial_split, "L+")
  checkmate::qassert(signals, "s+")
  checkmate::qassert(windows_set, "n+")

  data_pos_neg <- list()

  for (sig in signals) {
    cli::cli_h1("Processing signal {sig}")
    analysis_set <- rsample::analysis(initial_split[[sig]])

    # which classes are present in the dataset?
    classes <- unique(analysis_set$class)

    class_result <- list()

    # do the thing for each class
    for (cl in classes) {
      cli::cli_h2("Starting class {cl}")
      data_n <- analysis_set %>% dplyr::filter(!(class == cl & alarm == "true")) # including other classes "positives"
      data_p <- analysis_set %>% dplyr::filter(class == cl, alarm == "true")

      neg_stream <- NULL

      for (i in seq_len(nrow(data_n))) {
        neg_stream <- c(neg_stream, (data_n$values[[i]]))
      }

      if (validate) {
        neg_stream_val <- validate_data(neg_stream, windows_set)
      } else {
        neg_stream_val <- neg_stream
      }

      pos_stream <- NULL

      for (i in seq_len(nrow(data_p))) {
        pos_stream <- c(pos_stream, (data_p$values[[i]]))
      }

      if (validate) {
        pos_stream_val <- validate_data(pos_stream, windows_set)
      } else {
        pos_stream_val <- pos_stream
      }

      pos_mp <- mpx(pos_stream_val,
        windows_set,
        exclusion_zone = 0.5,
        distance = "euclidean",
        n_workers = n_workers,
        progress = FALSE
      )

      class_result[[cl]] <- list(
        neg_stream = neg_stream_val, pos_stream = pos_stream_val,
        pos_mp = pos_mp, window_size = windows_set
      )
    }
    data_pos_neg[[sig]] <- class_result
  }

  return(data_pos_neg)
}



find_snippets <- function(data_pos_neg, signals = "II", n_workers = 1) {
  checkmate::qassert(data_pos_neg, "L+")
  checkmate::qassert(signals, "s+")

  sig_snip <- list()

  for (sig in signals) {
    cli::cli_h1("Processing signal {sig}")

    # which classes are present in the dataset?
    classes <- unique(names(data_pos_neg[[sig]]))

    class_result <- list()

    # do the thing for each class
    for (cl in classes) {
      cli::cli_h2("Starting class {cl}")
      con <- contrast(data_pos_neg[[sig]][[cl]]$neg_stream, data_pos_neg[[sig]][[cl]]$pos_stream,
        data_pos_neg[[sig]][[cl]]$window_size, data_pos_neg[[sig]][[cl]]$pos_mp,
        exclusion_zone = 0.5, distance = "euclidean",
        n_workers = n_workers, progress = FALSE
      )
      class_result[[cl]] <- con
    }
    sig_snip[[sig]] <- class_result
  }

  return(sig_snip)
}
