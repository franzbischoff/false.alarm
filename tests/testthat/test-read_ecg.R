skip_on_ci()

library(here)
library(glue)
library(checkmate)

source(here("scripts", "common", "read_ecg.R"), encoding = "UTF-8")
dataset_path <- here("inst", "extdata", "physionet")

test_that("Can read ECG file", {
  data <- read_and_prepare_ecgs(glue("{dataset_path}/a103l"), normalize = TRUE)
  expect_equal(names(data[[1]]), c("time", "II", "V", "PLETH"))
  expect_equal(round(mean(data[[1]]$time) + sd(data[[1]]$time), 3), 260.261)
  expect_equal(round(mean(data[[1]]$II) + sd(data[[1]]$II), 3), 0.191) # here is the error
  expect_equal(round(mean(data[[1]]$V) + sd(data[[1]]$V), 3), 0.982)
  expect_equal(round(mean(data[[1]]$PLETH) + sd(data[[1]]$PLETH), 3), 0.574)
  expect_equal(attributes(data[[1]]$II)$info$gain, 7247)
  expect_equal(attributes(data[[1]])$info$alarm, "Asystole")
  qexpect(data, "L1")
  qexpect(data[[1]], "L4")
  qexpect(data[[1]]$time, "N82500")
  qexpect(data[[1]]$II, "N82500")
  qexpect(data[[1]]$V, "N82500")
  qexpect(data[[1]]$PLETH, "N82500")
})

test_that("Reshape truefalse", {
  data <- read_and_prepare_ecgs(glue("{dataset_path}/a103l"), normalize = TRUE)
  rdata <- reshape_ds_by_truefalse(data, "II")
  qexpect(rdata[[1]], "D")
  expect_equal(round(mean(rdata[[1]]$values$a103l) + sd(rdata[[1]]$values$a103l), 3), 0.191)
})
