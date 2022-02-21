#' Compute the Matrix Profile using batches, using the pre-computed statistics, for speed
#'
#' @param data_with_stats a `list` of size 2. Contains the data that will be used for computing the
#'   results and the pre-computed statistics.
#' @param params a `list` of parameters. More on details.
#' @param infos a `list` containing the attributes of the imported file and attributes added later
#'              in the pipeline. More on details.
#'
#' @details
#' The `params` for this function currently are:
#'
#' From mapped values (branches):
#' - window_size (integer)
#' - threshold (numeric), similarity threshold
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

compute_s_profile_with_stats <- function(data_with_stats, params, infos) {
  checkmate::qassert(data_with_stats, "L2")
  checkmate::assert_true(identical(
    attr(data_with_stats[[1]], "info"),
    attr(data_with_stats[[2]], "info")
  ))
  checkmate::qassert(infos, "L+")

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
    initial_stats, history = 0, mp_time_constraint = params$mp_time_constraint, progress = params$progress,
    threshold = params$threshold
  )

  new_data_vector <- seq.int(params$history + 1, length(data))

  "!DEBUG Data Size `length(data)`."

  new_data_list <- split(new_data_vector, ceiling(seq_along(new_data_vector) / params$batch))

  "!DEBUG Data List Size `length(new_data_list)`."

  profiles <- list()
  profiles[[1]] <- current_mp
  profiles[[1]]$motif_quality <- motif_quality(current_mp$right_matrix_profile,
    input_format = "pearson", window_size = params$window_size
  )
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
      mp_time_constraint = params$mp_time_constraint,
      progress = params$progress,
      threshold = params$threshold
    )

    profiles[[i]] <- current_mp
    profiles[[i]]$motif_quality <- motif_quality(current_mp$right_matrix_profile,
      input_format = "pearson", window_size = params$window_size
    )
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
