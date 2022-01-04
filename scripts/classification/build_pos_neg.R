build_pos_neg <- function(initial_split, signal = "II", snippet_size = 250,
                          validate = FALSE, same_class = TRUE) {
  checkmate::qassert(initial_split, "L+")
  checkmate::qassert(signal, "S1")
  checkmate::qassert(snippet_size, "N1")
  checkmate::qassert(validate, "B1")
  checkmate::qassert(same_class, "B1")

  cli::cli_h1("Processing signal {signal}, snippet size {snippet_size}")
  analysis_set <- rsample::analysis(initial_split[[signal]])

  # which classes are present in the dataset?
  classes <- unique(analysis_set$class)

  class_result <- list()

  # do the thing for each class
  for (cl in classes) {
    cli::cli_h2("Starting class {cl}")

    if (same_class) {
      data_n <- analysis_set %>% dplyr::filter(class == cl, alarm == "false")
    } else {
      data_n <- analysis_set %>% dplyr::filter(!(class == cl & alarm == "true")) # including other classes "positives"
    }
    data_p <- analysis_set %>% dplyr::filter(class == cl, alarm == "true")

    neg_stream <- NULL

    for (i in seq_len(nrow(data_n))) {
      neg_stream <- c(neg_stream, (data_n$values[[i]]))
    }

    if (validate) {
      neg_stream_val <- validate_data(neg_stream, snippet_size)
    } else {
      neg_stream_val <- neg_stream
    }

    pos_stream <- NULL

    for (i in seq_len(nrow(data_p))) {
      pos_stream <- c(pos_stream, (data_p$values[[i]]))
    }

    if (validate) {
      pos_stream_val <- validate_data(pos_stream, snippet_size)
    } else {
      pos_stream_val <- pos_stream
    }

    pos_mp <- mpx(pos_stream_val,
      snippet_size,
      exclusion_zone = 0.5,
      distance = "euclidean",
      progress = FALSE
    )

    class_result[[cl]] <- list(
      neg_stream = neg_stream_val, pos_stream = pos_stream_val,
      pos_mp = pos_mp, snippet_size = snippet_size
    )
  }

  data_pos_neg <- list()
  data_pos_neg[[signal]] <- class_result

  return(data_pos_neg)
}
