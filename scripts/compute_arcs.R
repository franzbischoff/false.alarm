compute_arcs2 <- function(ecg_data, exclude, time_constraint = NULL, exclusion_zone = NULL) {
  checkmate::qassert(ecg_data, "L")
  checkmate::qassert(exclusion_zone, c("0", "N"))
  checkmate::qassert(time_constraint, c("0", "N"))

  "!DEBUG Compute ARCS"

  file <- names(ecg_data)
  names <- names(ecg_data[[file]])
  names <- setdiff(names, exclude)

  ez <- exclusion_zone

  for (n in names) {
    mp <- attr(ecg_data[[file]][[n]], "mp")

    arc_counts <- vector(mode = "numeric")
    profile_index_size <- length(mp$profile_index)
    nnmark <- vector(mode = "numeric", profile_index_size)

    for (i in 1:profile_index_size) {
      j <- mp$profile_index[i]

      if (j == i) {
        # nnmark[j] <- nnmark[j] + 1
        next
      }

      if (!is.null(time_constraint)) {
        if (abs(j - i) > time_constraint) {
          next
        }
      }

      if (j < 0 || j > profile_index_size) {
        next
      }

      nnmark[min(i, j)] <- nnmark[min(i, j)] + 1
      nnmark[max(i, j)] <- nnmark[max(i, j)] - 1
    }

    arc_counts <- cumsum(nnmark)

    if (is.null(time_constraint)) {
      x <- seq(0, 1, length.out = profile_index_size)

      if (is.null(attr(mp, "subsetting"))) {
        ideal_arc_counts <- stats::dbeta(x, 2, 2) * profile_index_size / 3
      } else {
        ideal_arc_counts <- stats::dbeta(x, 2.1, 2.1) * profile_index_size / 3
      }

      if (is.null(exclusion_zone)) {
        ez <- mp$ez * 10 # normally ez is 0.5, so ez here is 5
      }

      corrected_arc_counts <- pmin(arc_counts / ideal_arc_counts, 1)
      ez <- round(mp$w * ez + .Machine$double.eps^0.5)
      corrected_arc_counts[1:min(ez, profile_index_size)] <- 1.0
      corrected_arc_counts[max((profile_index_size - ez + 1), 1):profile_index_size] <- 1.0
      mp$cac <- corrected_arc_counts
    } else {
      mp$cac <- arc_counts / max(arc_counts)
      mp$cac[1:min(time_constraint, profile_index_size)] <- 1.0
      mp$cac[max((profile_index_size - mp$w * 2 + 1), 1):profile_index_size] <- 1.0
    }

    attr(ecg_data[[file]][[n]], "mp") <- mp
  }

  return(ecg_data)
}
