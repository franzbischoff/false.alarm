#' Univariate STOMP algorithm
#'
#' Computes the Matrix Profile and Profile Index for Univariate Time Series.
#'
#' @details
#' The Matrix Profile, has the potential to revolutionize time series data mining because of its
#' generality, versatility, simplicity and scalability. In particular it has implications for time
#' series motif discovery, time series joins, shapelet discovery (classification), density
#' estimation, semantic segmentation, visualization, rule discovery, clustering etc. `verbose`
#' changes how much information is printed by this function; `0` means nothing, `1` means text, `2`
#' adds the progress bar, `3` adds the finish sound. `exclusion_zone` is used to avoid  trivial
#' matches; if a query data is provided (join similarity), this parameter is ignored.
#'
#' @param \dots a `matrix` or a `vector`. If a second time series is supplied it will be a join matrix
#'   profile.
#' @param window_size an `int`. Size of the sliding window.
#' @param exclusion_zone a `numeric`. Size of the exclusion zone, based on window size (default is
#'   `1/2`). See details.
#' @param verbose an `int`. See details. (Default is `2`).
#'
#' @return Returns a `MatrixProfile` object, a `list` with the matrix profile `mp`, profile index `pi`
#'   left and right matrix profile `lmp`, `rmp` and profile index `lpi`, `rpi`, window size `w` and
#'   exclusion zone `ez`.
#'
#' @family matrix profile computations
#'
#' @describeIn stomp Single thread version.
#'
#' @references * Zhu Y, Zimmerman Z, Senobari NS, Yeh CM, Funning G. Matrix Profile II : Exploiting
#'   a Novel Algorithm and GPUs to Break the One Hundred Million Barrier for Time Series Motifs and
#'   Joins. Icdm. 2016 Jan 22;54(1):739-48.
#' @references Website: <http://www.cs.ucr.edu/~eamonn/MatrixProfile.html>
#'
#' @examples
#' mp <- stomp(tsmp::mp_toy_data$data[1:200, 1], window_size = 30, verbose = 0)
#' \donttest{
#' ref_data <- tsmp::mp_toy_data$data[, 1]
#' query_data <- tsmp::mp_toy_data$data[, 2]
#' # self similarity
#' mp <- stomp(ref_data, window_size = 30)
#' # join similarity
#' mp2 <- stomp(ref_data, query_data, window_size = 30)
#' }
stompi2 <- function(data, window_size, exclusion_zone = 0.5, verbose = 2) {
  ez <- exclusion_zone # store original
  exclusion_zone <- round(window_size * exclusion_zone + .Machine$double.eps^0.5)
  data_size <- length(data)
  original_data <- data
  matrix_profile_size <- data_size - window_size + 1
  num_queries <- matrix_profile_size

  if (window_size > ceiling(data_size / 2)) {
    stop("Time series is too short relative to desired window size.", call. = FALSE)
  }
  if (window_size < 4) {
    stop("`window_size` must be at least 4.", call. = FALSE)
  }

  # check skip position
  skip_location <- rep(FALSE, matrix_profile_size)

  for (i in 1:matrix_profile_size) {
    if (any(is.na(data[i:(i + window_size - 1)])) || any(is.infinite(data[i:(i + window_size - 1)]))) {
      skip_location[i] <- TRUE
    }
  }

  # data[is.na(data) | is.infinite(data)] <- 0

  if (verbose > 1) {
    pb <- progress::progress_bar$new(
      format = "STOMP [:bar] :percent at :tick_rate it/s, elapsed: :elapsed, eta: :eta",
      clear = FALSE, total = num_queries, width = 80
    )
  }
  # forward
  nn <- false.alarm::mass_pre(data, window_size)
  dp <- false.alarm::mass(nn, data)

  first_product <- dp$last_product
  distance_profile <- dp$distance_profile
  last_product <- dp$last_product

  tictac <- Sys.time()

  matrix_profile <- rep_len(Inf, matrix_profile_size)
  profile_index <- rep_len(-Inf, matrix_profile_size)
  right_matrix_profile <- matrix_profile
  right_profile_index <- profile_index

  query_window <- data[1]
  drop_value <- 0

  for (i in 1:num_queries) {
    # compute the distance profile
    if (i > 1) {
      query_window <- data[i:(i + window_size - 1)]

      last_product[2:(data_size - window_size + 1)] <- last_product[1:(data_size - window_size)] -
        data[1:(data_size - window_size)] * drop_value +
        data[(window_size + 1):data_size] * query_window[window_size]

      last_product[1] <- first_product[i]
      distance_profile <- 2 * (window_size - (last_product - window_size * nn$data_mean * nn$query_mean[i]) /
        (nn$data_sd * nn$query_sd[i]))
    }

    distance_profile[distance_profile < 0] <- 0
    distance_profile <- sqrt(distance_profile)
    drop_value <- query_window[1]

    # apply exclusion zone
    if (exclusion_zone > 0) {
      exc_st <- max(1, i - exclusion_zone)
      exc_ed <- min(matrix_profile_size, i + exclusion_zone)
      distance_profile[exc_st:exc_ed] <- Inf
    }

    # exclude parts where the data_sd is "zero"
    distance_profile[nn$data_sd < .Machine$double.eps^0.5] <- Inf

    # exclude the whole distance profile if we are in the skip_location or the query_sd is "zero" TODO: can this be done early?
    if (skip_location[i] || any(nn$query_sd[i] < .Machine$double.eps^0.5)) {
      distance_profile[] <- Inf
    }

    # exclude parts where skip_location was marked
    distance_profile[skip_location] <- Inf

    # right matrix_profile
    ind <- (distance_profile[1:i] < right_matrix_profile[1:i])
    ind <- c(ind, rep(FALSE, matrix_profile_size - i)) # pad right
    right_matrix_profile[ind] <- distance_profile[ind]
    right_profile_index[which(ind)] <- i

    ind <- (distance_profile < matrix_profile)
    matrix_profile[ind] <- distance_profile[ind]
    profile_index[which(ind)] <- i

    if (verbose > 1) {
      pb$tick()
    }
  }

  tictac <- Sys.time() - tictac

  if (verbose > 0) {
    message(glue::glue("Finished in {glue_fmt('{tictac:.2f}')} {units(tictac)}"))
  }

  return({
    obj <- list(
      mp = matrix_profile, pi = profile_index,
      rmp = right_matrix_profile, rpi = right_profile_index,
      w = window_size,
      ez = ez,
      pars = list(
        data_fft = nn$data_fft,
        data_mean = nn$data_mean,
        data_sd = nn$data_sd,
        query_mean = nn$query_mean,
        query_sd = nn$query_sd,
        drop_value = drop_value,
        first_product = first_product,
        last_product = last_product,
        skip_location = skip_location
      ),
      data = list(original_data)
    )
    class(obj) <- "MatrixProfile"
    obj
  })
}
