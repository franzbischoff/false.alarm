## Tuning parameters

#' @export
trans_round <- function(d = 1.0) {
  force(d)

  d <- abs(d)

  if (d == 0.0) {
    cli::cli_warn(c("!" = "Rounding value 0 is invalid and was changed to 1."))
    d <- 1.0
  }

  roundi_trans <- function(x) {
    x + runif(1L, -1.0, 1.0) * (d / 2.0)
  }

  round_trans <- function(x) {
    round(x / d) * d
  }

  scales::trans_new(
    "trans_round",
    transform = "roundi_trans",
    inverse = "round_trans",
    domain = c(0.0, Inf)
  )
}

#' @export
time_constraint_par <- function(range = c(750L, 5000L), trans = trans_round(50L)) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    default = 5000L,
    label = c(time_constraint = "Time Constraint"),
    finalize = NULL
  )
}

#' @export
window_size_par <- function(range = c(150L, 250L), trans = trans_round(25L)) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    default = 200L,
    label = c(window_size = "Window Size"),
    finalize = NULL
  )
}

#' @export
mp_threshold_par <- function(range = c(0.0, 1.0), trans = trans_round(0.1)) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    default = 0.0,
    label = c(mp_threshold = "MP Threshold"),
    finalize = NULL
  )
}

#' @export
regime_threshold_par <- function(range = c(0.0, 1.0), trans = trans_round(0.05)) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    default = 0.3,
    label = c(regime_threshold = "Regime Threshold"),
    finalize = NULL
  )
}

#' @export
regime_landmark_par <- function(range = c(1.0, 10.0), trans = trans_round(0.5)) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    default = 3.0,
    label = c(regime_landmark = "Regime Landmark (s)"),
    finalize = NULL
  )
}

#' @export
# nolint start: object_name_linter
min_grid.floss_regime_model <- function(x, grid, ...) {
  # nolint end
  cli::cli_inform(c("*" = "min_grid.floss_regime_model "))
  # cli::cli_inform(c("*" = "min_grid.floss_regime_model: dots_n {rlang::dots_n(...)}"))
  if (rlang::dots_n(...) > 0L) {
    # 0L
    cli::cli_alert(c("*" = "min_grid.floss_regime_model: dots_names {names(rlang::dots_list(..., .preserve_empty = TRUE))}"))
  }

  # # This is basically `fit_max_value()` with an extra error trap
  gr_nms <- names(grid)
  param_info <- tune:::get_submodel_info(x)
  sub_nm <- param_info$id[param_info$has_submodel]

  if (length(sub_nm) == 0L || !any(names(grid) %in% sub_nm)) {
    return(blank_submodels(grid))
  }

  fixed_args <- gr_nms[!(gr_nms %in% sub_nm)]
  sub_nm <- sub_nm[(sub_nm %in% gr_nms)] # remove unnused submodels

  if (length(fixed_args) == 0L) {
    if (length(sub_nm) > 1L) {
      res <- tune:::submod_only_multi(grid)
    } else {
      res <- tune:::submod_only(grid)
    }
  } else {
    if (length(sub_nm) > 1L) {
      res <- tune:::submod_and_others_multi(grid, fixed_args)
    } else {
      res <- tune:::submod_and_others(grid, fixed_args)
    }
  }
  res
}

blank_submodels <- function(grid) {
  grid %>%
    dplyr::mutate(.submodels = purrr::map(
      seq_len(1L, nrow(grid)),
      ~ list()
    )) %>%
    dplyr::mutate_if(is.factor, as.character)
}
