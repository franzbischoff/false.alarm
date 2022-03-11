skip_on_ci()
skip_on_cran()

library(here)
# library(glue)
# library(checkmate)

source(here("scripts", "common", "validate_data.R"), encoding = "UTF-8")

test_that("Data is valid", {
  set.seed(2022)
  b <- validate_data(runif(1000), 10)
  expect_equal(round(mean(b) + sd(b), 3), 0.787)
})
