
fluss_extract <- function(ecg_data, exclude, num_segments = 3, exclusion_zone = NULL) {
  checkmate::qassert(ecg_data, "L")
  checkmate::qassert(num_segments, "N")
  checkmate::qassert(exclusion_zone, c("0", "N"))

  file <- names(ecg_data)
  checkmate::qassert(file, "S1")
  names <- names(ecg_data[[file]])
  names <- setdiff(names, exclude)

  for (n in names) {
    mp <- attr(ecg_data[[file]][[n]], "mp")

    if (is.null(exclusion_zone)) {
      ez <- mp$ez * 10 # normally ez is 0.5, so ez here is 5
    } else {
      ez <- exclusion_zone
    }

    cac <- mp$cac # keep cac intact
    segments_positions <- vector(mode = "numeric")
    arc_counts_size <- length(cac)
    ez <- round(mp$w * ez + .Machine$double.eps^0.5)

    for (i in 1:num_segments) {
      idx <- which.min(cac)

      if (cac[idx] >= 1) {
        break
      }
      segments_positions[i] <- idx
      cac[max(1, (idx - ez)):min(arc_counts_size, (idx + ez - 1))] <- Inf
    }

    mp$fluss <- segments_positions

    attr(ecg_data[[file]][[n]], "mp") <- mp
  }

  return(ecg_data)
}
