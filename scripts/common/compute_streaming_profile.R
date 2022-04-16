#' Compute the Matrix Profile using batches, without using pre-computed statistics
#'
#' @param ecg_data a numeric data containing the signal to be processed.
#' @param params a `list` of parameters. More on details.
#' @param infos a `list` containing the attributes of the imported file and attributes added later
#'              in the pipeline. More on details.
#'
#' @details
#' The `params` for this function currently are:
#'
#' From mapped values (branches):
#' - window_size (integer)
#' - mp_time_constraint (integer)
#'
#' Common values:
#' - ez (numeric), the exclusion zone to be used on MPX (proportion)
#' - batch (integer), the size of the batch for simulate the stream.
#' - history (integer), the size of the history buffer
#'
#' Specific values:
#' - progress (logical), TRUE for printing MPX progress.
#'
#' The `infos` is made available in case there is a need to access the attributes of the file that
#' contains the signal that is being processed
#'
#' @family process_ts_in_file
#'

compute_streaming_profile <- function(ecg_data, params, infos) {
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

  initial_mp$offset <- params$history

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
