process_ts_in_file <- function(ecg_data, exclude = "time", fun, params) {
  checkmate::qassert(ecg_data, "L")

  file <- names(ecg_data)
  checkmate::qassert(file, "S1")
  names <- names(ecg_data[[file]])
  names <- setdiff(names, exclude)

  params$exclude <- exclude

  for (n in names) {
    "!DEBUG Processing function `substitute(fun)`, `n` in `file`"
    res <- fun(ecg_data[[file]][[n]], params)
    ecg_data[[file]][[n]] <- res
  }

  return(ecg_data)
}
