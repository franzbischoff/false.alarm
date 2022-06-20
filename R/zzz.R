# nolint start
# nocov start

#' @importFrom rlang enquo
#' @importFrom purrr map_lgl
#' @importFrom tibble is_tibble as_tibble
#' @importFrom parsnip set_new_model
#' @importFrom parsnip multi_predict
#' @importFrom parsnip translate
#' @importFrom stats update
#' @importFrom withr with_options
#' @importFrom stats predict
#' @importFrom dials new_quant_param
#' @importFrom tune get_submodel_info submod_only submod_only_multi submod_and_others submod_and_others_multi

.onLoad <- function(libname, pkgname) {
  tryCatch(debugme::debugme(), error = identity)
  vctrs::s3_register("stats::predict", "floss_regime_model")
  vctrs::s3_register("false.alarm::print", "floss_regime_model")
  vctrs::s3_register("generics::min_grid", "floss_regime_model")
  vctrs::s3_register("stats::update", "floss_regime_model")
  vctrs::s3_register("parsnip::translate", "floss_regime_model")
  vctrs::s3_register("parsnip::multi_predict", "_floss_regime_model")
  vctrs::s3_register("false.alarm::floss_error", "data.frame")
  register_floss_regime_model()
  invisible()
}

.onAttach <- function(libname, pkgname) {
  Sys.setenv(RCPP_PARALLEL_BACKEND = "tbb")
  packageStartupMessage("Welcome to Matrix ProfileR")
  packageStartupMessage("Using TBB backend. Type help(false.alarm) for further help.")
}

.onUnload <- function(libpath) {
  unloadNamespace("debugme")
}
# nolint end

#' Returns a message to the user about function deprecation.
#'
#' Adapted from gg_dep from package ggplot2
#'
#' @param version The last version of false.alarm where this function was good (in other words,
#' the last version where it was not deprecated).
#' @param msg The message to print.
#'
#' @return Invisibly returns nothing
#' @keywords internal
#' @noRd
mp_dep <- function(version, msg) {
  v <- as.package_version(version)
  cv <- utils::packageVersion("false.alarm")

  # If current major number is greater than last-good major number, or if
  # current minor number is more than 1 greater than last-good minor number,
  # return an error.
  if (cv[[1L, 1L]] > v[[1L, 1L]] || cv[[1L, 2L]] > v[[1L, 2L]] + 1L) {
    stop(msg, " (Defunct; last used in version ", version, ")",
      call. = FALSE
    )

    # If minor number differs by one, give a warning
  } else if (cv[[1L, 2L]] > v[[1L, 2L]]) {
    warning(msg, " (Deprecated; last used in version ", version, ")",
      call. = FALSE
    )

    # If only subminor number is greater, provide a message
  } else if (cv[[1L, 3L]] > v[[1L, 3L]]) {
    message(msg, " (Deprecated; last used in version ", version, ")")
  }

  invisible()
}

# nocov end
