filter_stats <- function(data_filter, params) {
  checkmate::qassert(data_filter, "L2")
  checkmate::assert_true(identical(
    attr(data_filter[[1]], "info"),
    attr(data_filter[[2]], "info")
  ))

  stats <- data_filter[[1]]
  filter <- data_filter[[2]][[params$filter]]

  info <- attr(stats, "info")
  info$filter <- filter

  stats <- purrr::map(stats, function(x) x[!filter])

  attr(stats, "info") <- info

  # mp <- attr(ecg_data, params$attribute)
  # filter <- attr(ecg_data, "filters")
  # filter <- filter[[params$filter]]

  # mp$matrix_profile[filter] <- Inf
  # mp$profile_index[filter] <- which(filter)

  # attr(ecg_data, params$attribute) <- mp

  return(stats)
}
