compute_streaming_profile <- function(ecg_data, params) {
  checkmate::qassert(ecg_data, "N+")

  initial_data_vector <- seq.int(1, params$history)

  initial_mp <- mpx_stream_start(
    data = ecg_data[initial_data_vector], window_size = params$window_size,
    exclusion_zone = params$ez, mp_time_constraint = params$mp_time_constraint, progress = params$progress
  )

  "!DEBUG Data Size `length(ecg_data)`."

  new_data_vector <- seq.int(params$history + 1, length(ecg_data))
  new_data_list <- split(new_data_vector, ceiling(seq_along(new_data_vector) / params$batch))

  "!DEBUG Data List Size `length(new_data_list)`."

  "!DEBUG History `params$history`, batch size `params$batch`"

  initial_mp$offset <- params$history ## TODO: DEBUG

  profiles <- list()
  profiles[[1]] <- initial_mp
  i <- 2

  for (n in new_data_list) {
    "!!DEBUG Split `min(n)` to `max(n)`. History `params$history`, batch size `params$batch`"
    initial_mp <- mpx_stream_right(ecg_data[n], initial_mp, params$history, params$mp_time_constraint, params$progress)
    profiles[[i]] <- initial_mp
    i <- i + 1
  }

  "!DEBUG Finished `length(profiles)` profiles."

  attr(profiles, "info") <- attr(ecg_data, "info")

  return(profiles)
}
