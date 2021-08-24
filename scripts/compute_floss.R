compute_floss <- function(mp_data, params) {
  checkmate::qassert(mp_data, "L")
  ez <- params$ez
  time_constraint <- params$time_constraint
  threshold <- params$threshold

  checkmate::qassert(ez, c("0", "N"))

  info <- attr(data, "info")

  "!DEBUG Time constraint `time_constraint`."

  result_floss <- purrr::map(mp_data, function(x) {
    curr_ez <- ifelse(!is.null(ez), ez, x$ez * 10)
    cac <- compute_arcs(
      x$right_profile_index, x$w,
      curr_ez,
      time_constraint,
      threshold
    )
    list(cac = cac, w = x$w, ez = curr_ez, offset = x$offset)
  })

  "!DEBUG Finished `length(result_floss)` profiles."

  attr(result_floss, "info") <- info

  return(result_floss)
}

compute_arcs <- function(right_profile_index, window_size, exclusion_zone, time_constraint, threshold) {
  checkmate::qassert(right_profile_index, "N")
  checkmate::qassert(exclusion_zone, "N")

  "!!DEBUG Compute ARCS"

  ez <- round(exclusion_zone + .Machine$double.eps^0.5)
  arc_counts <- vector(mode = "numeric")
  cac_size <- length(right_profile_index)
  nnmark <- vector(mode = "numeric", cac_size)

  if (isTRUE(threshold)) {
    profile <- head(right_profile_index, -ez - 1)
    threshold <- max(abs(profile - seq_along(profile)))
  } else {
    threshold <- cac_size
  }

  for (i in seq.int(1, (cac_size - ez - 1))) {
    j <- right_profile_index[i]

    if (abs(j - i) <= threshold) {
      if (j == i) {
        next
      }

      # if (!is.null(time_constraint)) {
      #   if (abs(j - i) > time_constraint) {
      #     next
      #   }
      # }

      if (j < 0 || j > cac_size) {
        next
      }

      nnmark[min(i, j)] <- nnmark[min(i, j)] + 1
      nnmark[max(i, j)] <- nnmark[max(i, j)] - 1
    }
  }

  arc_counts <- cumsum(nnmark)

  if (time_constraint > 0) {
    cac <- arc_counts # / max(arc_counts)
    cac[seq.int(1, time_constraint)] <- NA
    cac[seq.int((cac_size - (3 * window_size) - 1), cac_size)] <- NA
    # cac[seq.int((cac_size - time_constraint - 1), cac_size)] <- 1.0
  } else {
    x <- seq(0, 1, length.out = cac_size)
    mode <- 0.6311142 # best point to analize the segment change
    a <- 1.939274
    b <- 1.69815
    ideal_arc_counts <- a * b * x^(a - 1) * (1 - x^a)^(b - 1) * cac_size / 4.035477 # nolint # kumaraswamy distribution

    cac <- pmin(arc_counts / ideal_arc_counts, 1) # below 1 or 1
    cac[seq.int(1, (2 * window_size))] <- 1.0
    cac[seq.int((cac_size - (3 * window_size) - 1), cac_size)] <- 1.0
    cac[cac < 0 | is.na(cac)] <- 1.0
  }

  return(cac)
}
