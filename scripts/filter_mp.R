filter_mp <- function(ecg_data, params) {
  mp <- attr(ecg_data, params$attribute)
  filter <- attr(ecg_data, "filters")
  filter <- filter[[params$filter]]

  mp$matrix_profile[filter] <- Inf
  mp$profile_index[filter] <- which(filter)

  attr(ecg_data, params$attribute) <- mp

  return(ecg_data)
}
