# an attempt to get the knee of a curve
kneed <- function(contrasts) {
  # contrasts <- c(0.31767218, 0.29146924, 0.17797986, 0.15451647, 0.09342605, 0.08841086, 0.08017338, 0.07088699, 0.07014500, 0.06976499)
  numk <- length(contrasts)
  first <- contrasts[1]
  last <- contrasts[numk]
  numer <- 1 / numk + contrasts[1]
  den <- 1 / numk + contrasts[1] + contrasts[numk]
  new_contrasts <- ((1 / numk + first) - contrasts) / (1 / numk + first - last)

  xlab <- seq_len(numk) / numk

  diag <- abs(xlab - new_contrasts) / sqrt(2)
  vert <- new_contrasts - xlab
  elb <- vert - diag

  return(elb)
}


get_exp_dist_series <- function(start, end, steps) {
  power_min <- log10(start)
  power_max <- log10(end)
  power_step <- (power_max - power_min) / steps
  powers <- seq(power_min, power_max, by = power_step)
  series <- unique(ceiling(10^powers))
  return(series)
}

# retrieve the pan contrast profile indexed by the k plato. position == 1 is the first pan-CP
get_topk_pan_contrast <- function(contrast_profiles, position = 1, repad = FALSE) {
  checkmate::qassert(contrast_profiles, "L+")

  "!DEBUG Get First Pan CP"
  w_sizes <- names(contrast_profiles)
  n_sizes <- as.numeric(w_sizes)
  cp_len <- nrow(contrast_profiles[[1]]$cps)

  pan_cp <- matrix(NA, ncol = length(w_sizes), nrow = cp_len)

  for (i in seq_len(length(w_sizes))) {
    # adding NA to the left makes a better pan plot, since we are working with streams, we look backwards
    if (repad == TRUE) {
      values <- c(rep(NA, n_sizes[i]), contrast_profiles[[i]]$cps[1:(cp_len - n_sizes[i]), position])
    } else {
      values <- c(contrast_profiles[[i]]$cps[1:(cp_len - n_sizes[i]), position], rep(NA, n_sizes[i]))
    }
    # pan_cp[, i] <- contrast_profiles[[i]]$cps[, 1]
    pan_cp[, i] <- values
    # print(paste("Plotting Pan Contrast Profile for shapelet size", i, "name", names(contrast_profiles[[i]])))
    # res <- pan_contrast[[as.character(i)]]
    # plot_contrast(res, i, num_shapelets)
  }


  colnames(pan_cp) <- w_sizes
  return(pan_cp)
}
