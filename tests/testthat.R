library(testthat)
library(false.alarm)

if (identical(Sys.getenv("NOT_CRAN"), "true") && !identical(Sys.getenv("TESTTHAT"), "false")) {
  test_check("false.alarm")
}
