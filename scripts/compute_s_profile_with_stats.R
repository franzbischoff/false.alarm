compute_s_profile_with_stats <- function(data_with_stats, params) {
  checkmate::qassert(data_with_stats, "L2")
  checkmate::assert_true(identical(
    attr(data_with_stats[[1]], "info"),
    attr(data_with_stats[[2]], "info")
  ))

  data <- data_with_stats[[1]]
  stats <- data_with_stats[[2]]

  # don't compute if the stats are not compatible
  checkmate::assert_true(
    attr(stats, "params")$window_size == params$window_size
  )

  "!DEBUG Constraint `params$constraint`, batch size `params$batch`"

  profile_len <- params$constraint - params$window_size + 1

  initial_data_vector <- seq.int(1, params$constraint)
  initial_stats_vector <- seq.int(1, profile_len)

  initial_stats <- purrr::map(stats, function(x) x[initial_stats_vector])
  initial_stats$ddf[profile_len] <- 0
  initial_stats$ddg[profile_len] <- 0

  initial_mp <- list(w = params$window_size, ez = params$ez, offset = 0)
  current_mp <- mpx_stream_s_right(data[initial_data_vector], batch_size = params$constraint, initial_mp, initial_stats, constraint = 0, progress = params$progress)

  new_data_vector <- seq.int(current_mp$offset + 1, length(data))
  new_data_list <- split(new_data_vector, ceiling(seq_along(new_data_vector) / params$batch))

  profiles <- list()
  profiles[[1]] <- current_mp
  i <- 2

  for (n in new_data_list) {
    batch <- length(n)
    start <- current_mp$offset - params$constraint + 1
    end <- current_mp$offset + batch
    data_vector <- seq.int(start, end)
    profile_len <- length(data_vector) - params$window_size + 1
    stats_vector <- seq.int(start, start + profile_len - 1)
    current_stats <- purrr::map(stats, function(x) x[stats_vector])
    current_stats$ddf[profile_len] <- 0
    current_stats$ddg[profile_len] <- 0
    current_mp <- mpx_stream_s_right(data[data_vector], batch_size = batch, current_mp, current_stats, params$constraint, params$progress)

    profiles[[i]] <- current_mp
    i <- i + 1
    "!!DEBUG batch size `batch`"
  }

  "!DEBUG Finished `length(profiles)` profiles."

  checkmate::assert_true(
    current_mp$offset == length(data)
  )

  attr(profiles, "info") <- attr(data, "info")

  return(profiles)
}
