"!DEBUG Loading target utils"

# Logics

`%|||%` <- tarchetypes:::`%|||%` # nolint

`%||NA%` <- tarchetypes:::`%||NA%` # nolint


# Language

as_symbols <- tarchetypes:::as_symbols

call_ns <- tarchetypes:::call_ns

call_function <- tarchetypes:::call_function

# Hacks

lst_to_df <- function(lst, keep_attributes = TRUE) {
  new_df <- dplyr::bind_rows(lst)

  if (keep_attributes) {
    nc <- nrow(new_df)
    attributes(new_df) <- attributes(lst[[1]])
    attr(new_df, "row.names") <- seq.int(1, nc)
  }

  return(new_df)
}

clean_splits_data <- function(object) {
  tidy_splits <- object$splits
  tidy_splits <- purrr::map(tidy_splits, function(x) {
    x$data$ts <- NA
    x
  })
  object$splits <- tidy_splits
  object
}
