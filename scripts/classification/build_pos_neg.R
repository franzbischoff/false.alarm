build_pos_neg <- function(initial_split, signal = "II", shapelet_sizes, positive = TRUE,
                          validate = FALSE, same_class = TRUE) {
  checkmate::qassert(initial_split, "L+")
  checkmate::qassert(signal, "S1")
  checkmate::qassert(shapelet_sizes, "N+")
  checkmate::qassert(positive, "B1")
  checkmate::qassert(validate, "B1")
  checkmate::qassert(same_class, "B1")

  cli::cli_h1("Processing signal {signal}, shapelet size(s) {shapelet_sizes}")
  analysis_set <- rsample::analysis(initial_split[[signal]])

  # which classes are present in the dataset?
  classes <- unique(analysis_set$class)

  class_result <- list()

  # do the thing for each class
  for (cl in classes) {
    cli::cli_h2("Starting class {cl}")

    shape_result <- list()
    for (shape in seq_along(shapelet_sizes)) {
      if (positive) { # Here, positive are the true alarms
        if (same_class) {
          data_n <- analysis_set %>% dplyr::filter(class == cl, alarm == "false")
        } else {
          data_n <- analysis_set %>% dplyr::filter(!(class == cl & alarm == "true")) # including other classes "positives"
        }
        data_p <- analysis_set %>% dplyr::filter(class == cl, alarm == "true")
      } else { # Here positive are the false alarms (so we try to get shapelets for the False alarm)
        if (same_class) {
          data_n <- analysis_set %>% dplyr::filter(class == cl, alarm == "true")
        } else {
          data_n <- analysis_set %>% dplyr::filter(!(class == cl & alarm == "false")) # including other classes "negatives"
        }
        data_p <- analysis_set %>% dplyr::filter(class == cl, alarm == "false")
      }

      neg_stream <- NULL

      for (i in seq_len(nrow(data_n))) {
        neg_stream <- c(neg_stream, (data_n$values[[i]]))
      }

      if (validate) {
        neg_stream_val <- validate_data(neg_stream, floor(shapelet_sizes[shape] / 2))
      } else {
        neg_stream_val <- neg_stream
      }

      pos_stream <- NULL

      for (i in seq_len(nrow(data_p))) {
        pos_stream <- c(pos_stream, (data_p$values[[i]]))
      }

      if (validate) {
        pos_stream_val <- validate_data(pos_stream, floor(shapelet_sizes[shape] / 2))
      } else {
        pos_stream_val <- pos_stream
      }

      shape_result[[shape]] <- list(
        neg_stream = neg_stream_val, pos_stream = pos_stream_val, shapelet_size = shapelet_sizes[shape]
      )
    }
    class_result[[cl]] <- shape_result
  }

  data_pos_neg <- list()
  data_pos_neg[[signal]] <- class_result

  return(data_pos_neg)
}
