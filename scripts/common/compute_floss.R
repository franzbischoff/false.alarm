#' Computes the FLOSS algorithm on a previous computed Matrix Profile
#'
#' @param mp_data a `list`. Contains the data that will be used for computing the results.
#' @param params a `list` of parameters. More on details.
#' @param infos a `list` containing the attributes of the imported file and attributes added later
#'              in the pipeline. More on details.
#'
#' @details
#' The `params` for this function currently are:
#'
#' From mapped values (branches):
#' - mp_time_constraint (integer) the time constraint applied on the Matrix Profile
#'
#' Common values:
#' - ez (integer) the exclusion zone used on the matrix profile (this is the actual number in sample unit)
#'
#' Specific values:
#' - floss_time_constraint (integer) the time constraint applied on FLOSS (it is overridden by the `mp_time_constraint`)
#' - sample_freq ?
#'
#' @family process_ts_in_file
#'

compute_floss <- function(mp_data, params, infos) {
  checkmate::qassert(mp_data, "L")
  checkmate::qassert(params, "L+")
  checkmate::qassert(infos, "L+")

  ez <- params$ez
  mp_time_constraint <- ifelse(is.null(params$mp_time_constraint), 0, params$mp_time_constraint)
  floss_time_constraint <- ifelse(is.null(params$floss_time_constraint), 0, params$floss_time_constraint)
  if (mp_time_constraint > floor(params$history * 3 / 4)) {
    mp_time_constraint <- 0
  }
  sample_freq <- params$sample_freq

  checkmate::qassert(ez, c("0", "N"))

  if (mp_time_constraint > 0 & floss_time_constraint > 0) {
    rlang::abort("You cannot set `mp_time_constraint` and `floss_time_constraint` at the same time.")
  }

  constraint <- FALSE

  if (mp_time_constraint > 0 || floss_time_constraint > 0) {
    constraint <- TRUE
  }

  info <- attr(mp_data, "info")

  if (constraint) {
    iac <- vector("list", 500)
    pro_size <- length(mp_data[[1]]$right_profile_index)
    for (i in 1:500) {
      iac[[i]] <- get_asym(pro_size, mp_time_constraint, floss_time_constraint)
    }

    aic_avg <- rowMeans(as.data.frame(iac))
    if (mp_time_constraint > 0 & mp_time_constraint < (pro_size / 2)) {
      aic_avg[seq.int(mp_time_constraint, pro_size - mp_time_constraint * 0.9)] <- mp_time_constraint / 2
    }
  } else {
    aic_avg <- NULL
  }

  result_floss <- purrr::map(mp_data, function(x) {
    curr_ez <- ifelse(!is.null(ez), ez, x$ez * 10)
    cac <- compute_arcs(
      x$right_profile_index, x$w,
      curr_ez,
      aic_avg,
      sample_freq,
      floss_time_constraint
    )

    if (isTRUE(params$cac_only)) {
      list(cac = cac$cac, w = x$w, ez = curr_ez, offset = x$offset)
    } else {
      list(cac = cac$cac, iac = cac$iac, arcs = cac$arcs, w = x$w, ez = curr_ez, offset = x$offset)
    }
  })

  "!DEBUG Finished `length(result_floss)` profiles."

  attr(result_floss, "info") <- info
  attr(result_floss, "params") <- params

  return(result_floss)
}

compute_arcs <- function(right_profile_index, window_size, exclusion_zone, aic_avg, sample_freq, floss_time_constraint) {
  checkmate::qassert(right_profile_index, "N")
  checkmate::qassert(exclusion_zone, "N")
  checkmate::assert_true(sample_freq > 50)

  "!!DEBUG Compute ARCS"

  ez <- round(exclusion_zone + .Machine$double.eps^0.5)
  arc_counts <- vector(mode = "numeric")
  cac_size <- length(right_profile_index)
  nnmark <- vector(mode = "numeric", cac_size)

  if (floss_time_constraint > 0) {
    constraint <- floss_time_constraint
  } else {
    constraint <- cac_size
  }

  for (i in seq.int(1, (cac_size - ez - 1))) {
    j <- right_profile_index[i]

    if (abs(j - i) <= constraint) {
      if (j == i) {
        next
      }

      if (j < 0 || j > cac_size) {
        next
      }

      nnmark[min(i, j)] <- nnmark[min(i, j)] + 1
      nnmark[max(i, j)] <- nnmark[max(i, j)] - 1
    }
  }

  arc_counts <- cumsum(nnmark)
  iac <- NULL

  if (!is.null(aic_avg)) {
    iac <- aic_avg
    cac <- pmin(arc_counts / iac, 1) # below 1 or 1
    cac[seq.int(1, (window_size / 2))] <- 1.0
    cac[seq.int((cac_size - window_size), cac_size)] <- 1.0
    cac[cac < 0 | is.na(cac)] <- 1.0
  } else {
    x <- seq(0, 1, length.out = cac_size)
    mode <- 0.6311142 # best point to analyze the segment change
    a <- 1.939274
    b <- 1.69815
    iac <- a * b * x^(a - 1) * (1 - x^a)^(b - 1) * cac_size / 4.035477 # nolint # kumaraswamy distribution

    cac <- pmin(arc_counts / iac, 1) # below 1 or 1
    cac[seq.int(1, (window_size / 2))] <- 1.0
    cac[seq.int((cac_size - window_size), cac_size)] <- 1.0
    cac[cac < 0 | is.na(cac)] <- 1.0
  }

  return(list(arcs = arc_counts, iac = iac, cac = cac))
}


get_asym <- function(pro_len = 50000, mp_tc = 0, floss_tc = 0) {
  mpi <- rep(0, pro_len)
  tc <- pro_len

  if (mp_tc > 0) {
    for (i in (1:(pro_len - 1))) {
      mpi[i] <- runif(1, i + 1, min(pro_len, i + mp_tc))
    }
  } else {
    # the same as for pure FLOSS without constraint. The constraint will be done later
    for (i in (1:(pro_len - 1))) {
      mpi[i] <- runif(1, i + 1, pro_len)
    }
    tc <- floss_tc
  }

  nnmark <- rep(0, pro_len)

  for (i in seq.int(1, pro_len - 1)) {
    j <- mpi[i]

    if (abs(j - i) <= tc) {
      if (j == i) {
        next
      }

      if (j < 0 || j > pro_len) {
        next
      }

      nnmark[min(i, j)] <- nnmark[min(i, j)] + 1
      nnmark[max(i, j)] <- nnmark[max(i, j)] - 1
    }
  }

  arc_counts <- cumsum(nnmark)
  arc_counts[pro_len] <- 0

  return(arc_counts)
}
