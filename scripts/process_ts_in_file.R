process_ts_in_file <- function(ecg_data, id, fun, params, exclude = "time") {
  checkmate::qassert(ecg_data, "L+")
  "!DEBUG receiving `length(ecg_data)` input."
  file <- unique(names(ecg_data))
  "!DEBUG file `file`."
  checkmate::qassert(file, "S1")

  fun_name <- substitute(fun)

  paas <- attr(ecg_data[[1]], "params")

  "!DEBUG assert paas: `paas$window_size`, params: `params$window_size`."
  "!DEBUG assert paas: `paas$time_constraint`, params: `params$time_constraint`."
  "!DEBUG assert paas: `paas$history`, params: `params$history`."

  if (!is.null(paas$window_size)) {
    checkmate::assert_true(identical(paas$window_size, params$window_size))
  }
  if (!is.null(paas$time_constraint)) {
    checkmate::assert_true(identical(paas$time_constraint, params$time_constraint))
  }
  if (!is.null(paas$history)) {
    checkmate::assert_true(identical(paas$history, params$history))
  }

  info <- attr(ecg_data[[1]], "info")
  info$id <- id
  info$ids <- c(id, info$ids)

  pars <- NULL
  if (length(ecg_data) > 1) {
    pars <- attr(ecg_data[[2]], "params")

    "!DEBUG assert pars: `pars$window_size`, params: `params$window_size`."
    "!DEBUG assert pars: `pars$time_constraint`, params: `params$time_constraint`."
    "!DEBUG assert pars: `pars$history`, params: `params$history`."

    if (!is.null(pars$window_size)) {
      checkmate::assert_true(identical(pars$window_size, params$window_size))
    }
    if (!is.null(pars$time_constraint)) {
      checkmate::assert_true(identical(pars$time_constraint, params$time_constraint))
    }
    if (!is.null(pars$history)) {
      checkmate::assert_true(identical(pars$history, params$history))
    }
  }

  ecg_data <- purrr::map(ecg_data, function(x) {
    names <- intersect(names(x), exclude)
    "!DEBUG excluding names `names`."
    for (n in names) {
      purrr::pluck(x, n) <- NULL
    }
    "!DEBUG keeping names `names(x)`."
    x
  })
  ecg_data <- purrr::transpose(ecg_data)

  params$exclude <- exclude

  result <- list()
  result[[file]] <- purrr::map(ecg_data, function(x) { ## attributes are lost with 2 lists
    "!DEBUG calling `fun_name`() for TS `names(x)`."

    if (checkmate::qtest(x, "L1")) {
      x <- x[[1]]
    }
    fun(x, params)
  })
  attr(result[[file]], "info") <- info

  endpars <- purrr::list_modify(params, !!!pars)
  attr(result[[file]], "params") <- endpars

  checkmate::qassert(result, "L1")

  "!DEBUG returning `length(result)` output."
  return(result)
}
