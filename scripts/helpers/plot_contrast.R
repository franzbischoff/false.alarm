plot_max_contrast <- function(contrast_profiles) {
  checkmate::qassert(contrast_profiles, "L")

  profile_size <- nrow(contrast_profiles[[1]]$cps)
  num_windows <- length(contrast_profiles)
  idxs <- vector("numeric", num_windows)
  values <- vector("numeric", num_windows)
  windows <- vector("numeric", num_windows)

  for (i in seq_len(num_windows)) {
    idxs[i] <- contrast_profiles[[i]]$plato_indices[1]
    values[i] <- contrast_profiles[[i]]$plato_primary_contrast[1]
    windows[i] <- nrow(contrast_profiles[[i]]$platos)
  }

  graphics::plot(
    x = idxs, y = windows, xlim = c(1, profile_size), type = "p",
    col = scales::alpha("red", values), pch = 19,
    xlab = "index", ylab = "Subsequence lenght", main = "Max Contrast by subsequence lenght"
  )
}
