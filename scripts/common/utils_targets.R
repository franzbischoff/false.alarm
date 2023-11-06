"!DEBUG Loading target utils"

# Logics

`%|||%` <- tarchetypes:::`%|||%` # nolint

`%||NA%` <- tarchetypes:::`%||NA%` # nolint

# Hacks

lst_to_df <- function(lst, keep_attributes = TRUE) {
  new_df <- dplyr::bind_rows(lst)

  if (keep_attributes) {
    nc <- nrow(new_df)
    attributes(new_df) <- attributes(lst[[1L]])
    attr(new_df, "row.names") <- seq.int(1L, nc) # nolint
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

pbFinished <- function(msg) { # nolint
  RPushbullet::pbPost("note", "Alert", msg)
}
