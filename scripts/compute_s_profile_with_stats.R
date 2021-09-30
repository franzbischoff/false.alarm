compute_s_profile_with_stats <- function(data_with_stats, params, infos) {
  checkmate::qassert(data_with_stats, "L2")
  checkmate::assert_true(identical(
    attr(data_with_stats[[1]], "info"),
    attr(data_with_stats[[2]], "info")
  ))

  data <- data_with_stats[[1]]
  data_info <- attr(data, "info")
  subset_start <- ifelse(isFALSE(data_info$subset), 0, data_info$subset[1] - 1)
  stats <- data_with_stats[[2]]

  # don't compute if the stats are not compatible
  checkmate::assert_true(
    attr(stats, "params")$window_size == params$window_size
  )

  "!DEBUG History `params$history`, batch size `params$batch`"

  profile_len <- params$history - params$window_size + 1

  initial_data_vector <- seq.int(1, params$history)
  initial_stats_vector <- seq.int(1, profile_len)

  initial_stats <- purrr::map(stats, function(x) x[initial_stats_vector])
  initial_stats$ddf[profile_len] <- 0
  initial_stats$ddg[profile_len] <- 0

  initial_mp <- list(w = params$window_size, ez = params$ez, offset = subset_start)
  current_mp <- mpx_stream_s_right(data[initial_data_vector],
    batch_size = params$history, initial_mp,
    initial_stats, history = 0, time_constraint = params$time_constraint, progress = params$progress,
    threshold = params$threshold
  )

  new_data_vector <- seq.int(params$history + 1, length(data))

  "!DEBUG Data Size `length(data)`."

  new_data_list <- split(new_data_vector, ceiling(seq_along(new_data_vector) / params$batch))

  "!DEBUG Data List Size `length(new_data_list)`."

  profiles <- list()
  profiles[[1]] <- current_mp
  i <- 2

  for (n in new_data_list) {
    batch <- length(n)
    start <- (current_mp$offset - subset_start) - params$history + 1
    end <- (current_mp$offset - subset_start) + batch
    data_vector <- seq.int(start, end)
    profile_len <- length(data_vector) - params$window_size + 1
    stats_vector <- seq.int(start, start + profile_len - 1)
    current_stats <- purrr::map(stats, function(x) x[stats_vector])
    current_stats$ddf[profile_len] <- 0
    current_stats$ddg[profile_len] <- 0
    current_mp <- mpx_stream_s_right(data[data_vector],
      batch_size = batch, current_mp,
      current_stats, history = params$history,
      time_constraint = params$time_constraint,
      progress = params$progress,
      threshold = params$threshold
    )

    profiles[[i]] <- current_mp
    i <- i + 1
    "!!DEBUG batch size `batch`"
  }

  "!DEBUG Finished `length(profiles)` profiles."

  checkmate::assert_true(
    current_mp$offset == length(data) + subset_start
  )

  attr(profiles, "info") <- data_info

  return(profiles)
}
