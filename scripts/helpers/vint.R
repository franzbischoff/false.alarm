source(here::here("scripts", "helpers", "interactions.R"))
# vint <- function(
#     object, feature_names, progress = "none", parallel = FALSE,
#     paropts = NULL, ...) {
#   all.pairs <- utils::combn(feature_names, m = 2)
#   ints <- plyr::aaply(all.pairs,
#     .margins = 2, .progress = progress,
#     .parallel = parallel, .paropts = paropts, .fun = function(x) {
#       pd <- pdp::partial(object, pred.var = x, ...)
#       mean(c(stats::sd(tapply(pd$yhat,
#         INDEX = pd[[x[1L]]],
#         FUN = stats::sd
#       )), stats::sd(tapply(pd$yhat,
#         INDEX = pd[[x[2L]]], FUN = stats::sd
#       ))))
#     }
#   )
#   ints <- data.frame(Variables = paste0(
#     all.pairs[1L, ], "*",
#     all.pairs[2L, ]
#   ), Interaction = ints)
#   # ints <- ints[order(ints["Interaction"], decreasing = TRUE), ] # this retuns a bug
#   ints <- tibble::as_tibble(ints)
#   ints <- dplyr::arrange(ints, dplyr::desc(Interaction))
#   ints
# }
