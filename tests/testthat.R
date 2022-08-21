library(testthat) # nolint
library(false.alarm) # nolint

if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  # emulates `testthat:::on_cran()`
  if (requireNamespace("xml2")) {
    test_check("false.alarm", reporter = MultiReporter$new(reporters = list(JunitReporter$new(file = "test-results.xml"), CheckReporter$new())))
  } else {
    test_check("false.alarm")
  }
}
