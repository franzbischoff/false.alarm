skip_on_ci()

source(here::here("scripts/stompi2.R"), encoding = "UTF-8")
source(here::here("scripts/stompi2_update.R"), encoding = "UTF-8")
source(here::here("scripts/stompi3_update.R"), encoding = "UTF-8")
source(here::here("R/stomp.R"), encoding = "UTF-8")


data <- as.numeric(tsmp::mp_fluss_data$tilt_abp$data[1:1000])
data1 <- as.numeric(tsmp::mp_fluss_data$tilt_abp$data[1:1001])
data2 <- as.numeric(tsmp::mp_fluss_data$tilt_abp$data[1:1010])
data3 <- as.numeric(tsmp::mp_fluss_data$tilt_abp$data[111:1010])
newdata <- as.numeric(tsmp::mp_fluss_data$tilt_abp$data[1001:1010])

test_that("Stomp works", {
  start <- stompi2(data, window_size = 200, verbose = 0)
  start2 <- tsmp::stomp(data, window_size = 200)
  expect_equal(as.numeric(start$mp), as.numeric(start2$mp))
  expect_equal(as.numeric(start$rmp), as.numeric(start2$rmp))
})

test_that("Normal Stomp + update batch", {
  start <- stompi2(data, window_size = 200, verbose = 0)
  update <- stompi2_update(start, newdata)
  update2 <- stompi2_update(start, newdata[1])
  update2 <- stompi2_update(update2, newdata[2])
  update2 <- stompi2_update(update2, newdata[3])
  update2 <- stompi2_update(update2, newdata[4])
  update2 <- stompi2_update(update2, newdata[5])
  update2 <- stompi2_update(update2, newdata[6])
  update2 <- stompi2_update(update2, newdata[7])
  update2 <- stompi2_update(update2, newdata[8])
  update2 <- stompi2_update(update2, newdata[9])
  update2 <- stompi2_update(update2, newdata[10])
  update3 <- tsmp::stomp(data2, window_size = 200)

  expect_equal(as.numeric(update$mp), as.numeric(update2$mp))
  expect_equal(as.numeric(update$rmp), as.numeric(update2$rmp))
  expect_equal(as.numeric(update3$mp), as.numeric(update$mp))
  expect_equal(as.numeric(update3$rmp), as.numeric(update$rmp))
})

test_that("Normal Stomp + update buffer", {
  start <- stompi2(data, window_size = 200, verbose = 0)
  update4 <- stompi2_update(start, newdata, history_size = 900)
  update5 <- tsmp::stomp(data3, window_size = 200)

  expect_false(isTRUE(all.equal(as.numeric(update4$mp), as.numeric(update5$mp))))
  expect_equal(as.numeric(update4$rmp), as.numeric(update5$rmp))
})

test_that("Normal Stomp + update batch2", {
  start <- stompi2(data, window_size = 200, verbose = 0)
  start1 <- stompi2(data1, window_size = 200, verbose = 0)
  update6 <- stompi3_update(start, newdata[1])
  update7 <- stompi3_update(update6, newdata[2])
  update7 <- stompi3_update(update7, newdata[3])
  update7 <- stompi3_update(update7, newdata[4])
  update7 <- stompi3_update(update7, newdata[5])
  update7 <- stompi3_update(update7, newdata[6])
  update7 <- stompi3_update(update7, newdata[7])
  update7 <- stompi3_update(update7, newdata[8])
  update7 <- stompi3_update(update7, newdata[9])
  update7 <- stompi3_update(update7, newdata[10])
  update3 <- tsmp::stomp(data2, window_size = 200)
  # update6 <- stompi3_update(start, newdata)

  expect_equal(as.numeric(start1$mp), as.numeric(update6$mp))
  expect_equal(as.numeric(start1$rmp), as.numeric(update6$rmp))
  expect_equal(as.numeric(update7$mp), as.numeric(update3$mp))
  expect_equal(as.numeric(update7$rmp), as.numeric(update3$rmp))
})
