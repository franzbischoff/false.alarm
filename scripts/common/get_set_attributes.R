get_info <- function(obj) {
  return(attr(obj, "info"))
}

get_params <- function(obj) {
  return(attr(obj, "params"))
}

get_file_annotations <- function(obj) {
  checkmate::qassert(obj, "L+")

  depth <- purrr::vec_depth(obj)
  if (!is.null(attr(obj, "annotations"))) {
    return(attr(obj, "annotations"))
  } else if (depth > 1) {
    for (i in seq.int(1, depth - 1)) {
      obj <- obj[[1]]
      if (!is.null(attr(obj, "annotations"))) {
        return(attr(obj, "annotations"))
      }
    }
  }

  return(NULL)
}

get_signal_info <- function(obj, sig) {
  checkmate::qassert(obj, "L+")
  checkmate::qassert(sig, "S")
  attr(rlist::list.search(obj, attr(., "info")$signal == sig)[[1]], "info")
}

get_file_info <- function(obj) {
  checkmate::qassert(obj, "L+")

  depth <- purrr::vec_depth(obj)
  if (!is.null(attr(obj, "info")$filename)) {
    return(attr(obj, "info"))
  } else if (depth > 1) {
    for (i in seq.int(1, depth - 1)) {
      obj <- obj[[1]]
      if (!is.null(attr(obj, "info")$filename)) {
        return(attr(obj, "info"))
      }
    }
  }

  return(NULL)
}
