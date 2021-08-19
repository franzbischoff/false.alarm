process_ts_in_file <- function(ecg_data, id, fun, params, exclude = "time") {
  checkmate::qassert(ecg_data, "L+")
  "!DEBUG receiving `length(ecg_data)` input."
  file <- unique(names(ecg_data))
  "!DEBUG file `file`."
  checkmate::qassert(file, "S1")

  fun_name <- substitute(fun)

  info <- attr(ecg_data[[1]], "info")
  info$id <- id
  info$ids <- c(id, info$ids)

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
  attr(result[[file]], "params") <- params

  checkmate::qassert(result, "L1")

  "!DEBUG returning `length(result)` output."
  return(result)
}
