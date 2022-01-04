# the seed is binded to the target hash value, so this is reproductible
# We could just apply initial_split over the `dataset`, but the `strata` argument
# does not reach the `alarm` column that we use to stratify the sampling.
# The stratification is done to keep classes, TRUEs and FALSEs proportional to the original dataset

build_initial_split <- function(ecg_data, prop = 0.75, strata = "class_alarm", signals = NULL) {
  checkmate::qassert(ecg_data, "L+")
  checkmate::qassert(prop, "N(0,1)")
  checkmate::qassert(strata, "S1")
  checkmate::qassert(signals, "s+")

  # get the seed used on this target
  seed <- tar_seed()

  if (is.null(signals)) {
    signals <- names(ecg_data)

    if (is.null(signals)) {
      signals <- 1
    }
  }

  result <- list()

  for (i in signals) {
    set.seed(seed) # ensure that the splits are the same throughout the included data series
    result[[i]] <- rsample::initial_split(ecg_data[[i]], prop = prop, strata = all_of(strata))
  }
  # return the compiled list
  result
}
