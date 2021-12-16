# the seed is binded to the target hash value, so this is reproductible
# We could just apply initial_split over the `dataset`, but the `strata` argument
# does not reach the `alarm` column that we use to stratify the sampling.
# The stratification is done to keep TRUEs and FALSEs proportional to the original dataset

build_initial_split <- function(ecg_data, prop = 0.75, strata = "alarm", include = NULL) {
  checkmate::qassert(ecg_data, "D+")
  checkmate::qassert(prop, "N(0,1)")
  checkmate::qassert(strata, "S1")
  checkmate::qassert(include, "s+")

  # get the seed used on this target
  seed <- tar_seed()

  if (is.null(include)) {
    include <- names(ecg_data)

    if (is.null(include)) {
      include <- 1
    }
  }

  result <- list()

  for (i in include) {
    set.seed(seed) # ensure that the splits are the same throughout the included data series
    result[[i]] <- rsample::initial_split(ecg_data[[i]], prop = prop, strata = all_of(strata))
  }
  # return the compiled list
  result
}
