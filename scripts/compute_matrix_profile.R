compute_matrix_profile <- function(ecg_data, params) {
  res <- false.alarm::mpx(ecg_data,
    window_size = params$window_size,
    exclusion_zone = params$ez,
    s_size = params$s_size,
    idxs = params$idxs,
    distance = params$distance,
    n_workers = 1,
    progress = params$progress,
    constraint = params$constraint
  )
  res$constraint <- params$constraint
  res$w <- params$window_size
  res$ez <- params$ez
  attr(ecg_data, params$attribute) <- res

  return(ecg_data)
}
