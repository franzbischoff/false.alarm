# skip_on_ci()

library(here)
# library(glue)
# library(checkmate)

source(here("scripts", "common", "win_complex.R"), encoding = "UTF-8")

test_that("Data is valid", {
  set.seed(2022)
  b <- win_complex(runif(1000), 10)
  expect_equal(round(mean(b) + sd(b), 3), 1.474)
})
