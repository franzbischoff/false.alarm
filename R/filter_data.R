#' filter_data
#'
#' TODO: NA values invalidates a whole window size; remember this on online algorithm
#'
#' @export

filter_data <- function(ecg_data) {
  checkmate::qassert(ecg_data, "L")

  file <- names(ecg_data)
  checkmate::qassert(file, "S1")
  names <- names(ecg_data[[file]])
  names <- setdiff(names, "time")

  for (n in names) {

    mp <- attr(ecg_data[[file]][[n]], "mp")
    norm_data <- matrixprofiler::znorm(ecg_data[[file]][[n]])
    av <- matrixprofiler::mov_std(norm_data, mp$w)
    mu <- mean(av)

    filter_vector <- (av >= mu)
    mp$av <- filter_vector
    mp$matrix_profile[filter_vector] <- Inf
    mp$profile_index[filter_vector] <- which(filter_vector)

    attr(ecg_data[[file]][[n]], "mp") <- mp
  }

  return(ecg_data)
}
