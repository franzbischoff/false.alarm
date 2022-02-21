#' Apply a pre-processed filter to the data
#'
#' @param data_filter a `list` with two items, the data and the filter to be applied.
#' @param params a `list` of parameters. More on details.
#' @param infos a `list` containing the attributes of the imported file and attributes added later
#'              in the pipeline. More on details.
#'
#' @details
#' The `params` for this function currently are:
#'
#' From mapped values (branches):
#' - filter (string), the name of the filter
#'
#' The `infos` is made available in case there is a need to access the attributes of the file that
#' contains the signal that is being processed
#'
#' @family process_ts_in_file
#'

filter_data <- function(data_filter, params, infos) {
  checkmate::qassert(data_filter, "L2")
  checkmate::assert_true(identical(
    attr(data_filter[[1]], "info"),
    attr(data_filter[[2]], "info")
  ))

  data <- data_filter[[1]]
  filter <- data_filter[[2]][[params$filter]]
  data <- data[!filter]
  info <- attr(data_filter[[1]], "info")
  info$filter <- filter
  attr(data, "info") <- info
  attr(data, "params") <- params

  # mp <- attr(ecg_data, params$attribute)
  # filter <- attr(ecg_data, "filters")
  # filter <- filter[[params$filter]]

  # mp$matrix_profile[filter] <- Inf
  # mp$profile_index[filter] <- which(filter)

  # attr(ecg_data, params$attribute) <- mp

  return(data)
}
