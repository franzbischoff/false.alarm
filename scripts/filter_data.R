filter_data <- function(data_filter, params) {
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

  # mp <- attr(ecg_data, params$attribute)
  # filter <- attr(ecg_data, "filters")
  # filter <- filter[[params$filter]]

  # mp$matrix_profile[filter] <- Inf
  # mp$profile_index[filter] <- which(filter)

  # attr(ecg_data, params$attribute) <- mp

  return(data)
}
