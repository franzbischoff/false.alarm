compute_streaming_profile <- function(ecg_data, params) {
  checkmate::qassert(ecg_data, "N+")

  initial_data_vector <- seq.int(1, params$constraint)

  initial_mp <- mpx_stream_start(ecg_data[initial_data_vector], params$window_size, params$ez, FALSE)

  new_data_vector <- seq.int(params$constraint, length(ecg_data))

  new_data_list <- split(new_data_vector, ceiling(seq_along(new_data_vector) / params$batch))

  "!DEBUG Constraint `params$constraint`, batch size `params$batch`"

  initial_mp$offset <- params$constraint

  profiles <- list()
  profiles[[1]] <- initial_mp
  i <- 2

  for (n in new_data_list) {
    "!!DEBUG Split `min(n)` to `max(n)`. Constraint `params$constraint`, batch size `params$batch`"
    initial_mp <- mpx_stream_right(ecg_data[n], initial_mp, params$constraint, params$progress)

    profiles[[i]] <- initial_mp
    i <- i + 1
  }

  "!DEBUG Finished `length(profiles)` profiles."

  attr(profiles, "info") <- attr(ecg_data, "info")

  return(profiles)
}
