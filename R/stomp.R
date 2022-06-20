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
#' @param mp_time_constraint an `int`. Max distance where to look for the best match in matrix profile.
#' (default is NULL).
#' @param verbose an `int`. See details. (Default is `2`).
#'
#' @return Returns a `MatrixProfile` object, a `list` with the matrix profile `mp`, profile index `pi`
#'   left and right matrix profile `lmp`, `rmp` and profile index `lpi`, `rpi`, window size `w` and
#'   exclusion zone `ez`.
#'
#' @export
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
stomp <- function(..., window_size, exclusion_zone = getOption("tsmp.exclusion_zone", 0.5), mp_time_constraint = NULL,
                  verbose = getOption("tsmp.verbose", 2L)) {
  argv <- list(...)
  argc <- length(argv)
  data <- argv[[1L]]
  if (argc > 1L && !is.null(argv[[2L]])) {
    query <- argv[[2L]]
    exclusion_zone <- 0.0 # don't use exclusion zone for joins
    join <- TRUE
  } else {
    query <- data
    join <- FALSE
  }

  # transform data into matrix
  if (is.vector(data)) {
    data <- as.matrix(data)
  } else if (is.matrix(data)) {
    if (ncol(data) > nrow(data)) {
      data <- t(data)
    }
  } else {
    stop("Unknown type of data. Must be: a column matrix or a vector.", call. = FALSE)
  }

  if (is.vector(query)) {
    query <- as.matrix(query)
  } else if (is.matrix(query)) {
    if (ncol(query) > nrow(query)) {
      query <- t(query)
    }
  } else {
    stop("Unknown type of query. Must be: a column matrix or a vector.", call. = FALSE)
  }

  ez <- exclusion_zone # store original
  exclusion_zone <- round(window_size * exclusion_zone + .Machine$double.eps^0.5)
  data_size <- nrow(data)
  query_size <- nrow(query)
  matrix_profile_size <- data_size - window_size + 1L
  num_queries <- query_size - window_size + 1L

  if (query_size > data_size) {
    stop("Query must be smaller or the same size as reference data.")
  }
  if (window_size > ceiling(query_size / 2.0)) {
    stop("Time series is too short relative to desired window size.", call. = FALSE)
  }
  if (window_size < 4L) {
    stop("`window_size` must be at least 4.", call. = FALSE)
  }

  # check skip position
  skip_location <- rep(FALSE, matrix_profile_size)

  for (i in 1L:matrix_profile_size) {
    if (anyNA(data[i:(i + window_size - 1L)]) || any(is.infinite(data[i:(i + window_size - 1L)]))) {
      skip_location[i] <- TRUE
    }
  }

  data[is.na(data)] <- 0.0
  data[is.infinite(data)] <- 0.0

  query[is.na(query)] <- 0.0
  query[is.infinite(query)] <- 0.0

  if (verbose > 1L) {
    pb <- progress::progress_bar$new(
      format = "STOMP [:bar] :percent at :tick_rate it/s, elapsed: :elapsed, eta: :eta",
      clear = FALSE, total = num_queries, width = 80L
    )
  }

  first_product <- matrix(0.0, num_queries, 1L)

  # forward
  nn <- tsmp::dist_profile(data, query, window_size = window_size)
  # reverse
  # This is needed to handle with the join similarity.
  rnn <- tsmp::dist_profile(query, data, window_size = window_size)

  first_product[, 1L] <- rnn$last_product

  tictac <- Sys.time()

  matrix_profile <- matrix(Inf, matrix_profile_size, 1L)
  profile_index <- matrix(-Inf, matrix_profile_size, 1L)
  if (join) {
    # no RMP and LMP for joins
    left_matrix_profile <- right_matrix_profile <- NULL
    left_profile_index <- right_profile_index <- NULL
  } else {
    left_matrix_profile <- right_matrix_profile <- matrix_profile
    left_profile_index <- right_profile_index <- profile_index
  }
  distance_profile <- matrix(0.0, matrix_profile_size, 1L)
  last_product <- matrix(0.0, matrix_profile_size, 1L)
  drop_value <- matrix(0.0, 1L, 1L)

  debug <- list()

  for (i in 1L:num_queries) {
    # compute the distance profile
    query_window <- as.matrix(query[i:(i + window_size - 1L), 1L])


    if (i == 1L) {
      distance_profile[, 1L] <- nn$distance_profile
      last_product[, 1L] <- nn$last_product
    } else {
      last_product[2L:(data_size - window_size + 1L), 1L] <- last_product[1L:(data_size - window_size), 1L] -
        data[1L:(data_size - window_size), 1L] * drop_value +
        data[(window_size + 1L):data_size, 1L] * query_window[window_size, 1L]

      last_product[1L, 1L] <- first_product[i, 1L]
      distance_profile <- 2L * (window_size - (last_product - window_size * nn$par$data_mean * nn$par$query_mean[i]) /
        (nn$par$data_sd * nn$par$query_sd[i]))
    }

    debug[[i]] <- as.numeric(query_window)

    distance_profile[distance_profile < 0.0] <- 0.0
    distance_profile <- sqrt(distance_profile)
    drop_value <- query_window[1L, 1L]

    # apply exclusion zone
    if (exclusion_zone > 0.0) {
      exc_st <- max(1.0, i - exclusion_zone)
      exc_ed <- min(matrix_profile_size, i + exclusion_zone)
      distance_profile[exc_st:exc_ed, 1L] <- Inf
    }

    # time constraint:
    if (!is.null(mp_time_constraint)) {
      exc_st <- 1L:max(1.0, i - mp_time_constraint)
      exc_ed <- min(matrix_profile_size, i + mp_time_constraint):matrix_profile_size

      distance_profile[c(exc_st, exc_ed), 1L] <- Inf
    }

    distance_profile[nn$par$data_sd < .Machine$double.eps^0.5] <- Inf
    if (skip_location[i] || any(nn$par$query_sd[i] < .Machine$double.eps^0.5)) {
      distance_profile[] <- Inf
    }
    distance_profile[skip_location] <- Inf

    if (!join) {
      # no RMP and LMP for joins
      # left matrix_profile
      ind <- (distance_profile[i:matrix_profile_size] < left_matrix_profile[i:matrix_profile_size])
      ind <- c(rep(FALSE, (i - 1L)), ind) # pad left
      left_matrix_profile[ind] <- distance_profile[ind]
      left_profile_index[which(ind)] <- i

      # right matrix_profile
      ind <- (distance_profile[1L:i] < right_matrix_profile[1L:i])
      ind <- c(ind, rep(FALSE, matrix_profile_size - i)) # pad right
      right_matrix_profile[ind] <- distance_profile[ind]
      right_profile_index[which(ind)] <- i
    }

    ind <- (distance_profile < matrix_profile)
    matrix_profile[ind] <- distance_profile[ind]
    profile_index[which(ind)] <- i

    if (verbose > 1L) {
      pb$tick()
    }
  }

  tictac <- Sys.time() - tictac

  if (verbose > 0L) {
    message(glue::glue("Finished in {glue_fmt('{tictac:.2f}')} {units(tictac)}"))
  }

  return({
    obj <- list(
      mp = matrix_profile, pi = profile_index,
      rmp = right_matrix_profile, rpi = right_profile_index,
      lmp = left_matrix_profile, lpi = left_profile_index,
      w = window_size,
      ez = ez,
      pars = list(
        debug = debug
      )
    )
    class(obj) <- "MatrixProfile"
    attr(obj, "join") <- join
    obj
  })
}
