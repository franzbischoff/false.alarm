datal0 <- rev(as.numeric(tsmp::mp_fluss_data$tilt_abp$data)[5001:6000])
datal01 <- rev(as.numeric(tsmp::mp_fluss_data$tilt_abp$data)[5001:6001])
datal02 <- rev(as.numeric(tsmp::mp_fluss_data$tilt_abp$data)[5001:6002])
datal0100 <- rev(as.numeric(tsmp::mp_fluss_data$tilt_abp$data)[5001:6100])
datar0 <- (as.numeric(tsmp::mp_fluss_data$tilt_abp$data)[5001:6000])
datar01 <- (as.numeric(tsmp::mp_fluss_data$tilt_abp$data)[5001:6001])
datar02 <- (as.numeric(tsmp::mp_fluss_data$tilt_abp$data)[5001:6002])
datar0100 <- (as.numeric(tsmp::mp_fluss_data$tilt_abp$data)[5001:6100])
tester0 <- mpx_rcpp(datar0, 80, 0.5, 1.0, TRUE, FALSE, FALSE)
tester01 <- mpx_rcpp(datar01, 80, 0.5, 1.0, TRUE, FALSE, FALSE)
tester02 <- mpx_rcpp(datar02, 80, 0.5, 1.0, TRUE, FALSE, FALSE)
tester0100 <- mpx_rcpp(datar0100, 80, 0.5, 1.0, TRUE, FALSE, FALSE)
testel0 <- mpx_rcpp(datal0, 80, 0.5, 1.0, TRUE, FALSE, FALSE)
testel01 <- mpx_rcpp(datal01, 80, 0.5, 1.0, TRUE, FALSE, FALSE)
testel02 <- mpx_rcpp(datal02, 80, 0.5, 1.0, TRUE, FALSE, FALSE)
testel0100 <- mpx_rcpp(datal0100, 80, 0.5, 1.0, TRUE, FALSE, FALSE)

test_that("Normal mpx + update batch right", {
  tester001 <- mpxiright_rcpp(datar01, 80, 0.5, 1.0, TRUE, FALSE, FALSE, 1, tester0) # 881
  tester002 <- mpxiright_rcpp(datar02, 80, 0.5, 1.0, TRUE, FALSE, FALSE, 2, tester0) # 1763
  tester00100 <- mpxiright_rcpp(datar0100, 80, 0.5, 1.0, TRUE, FALSE, FALSE, 100, tester0) # 93050

  expect_equal(as.numeric(tester01$right_profile_index), as.numeric(tester001$right_profile_index))
  expect_equal(as.numeric(tester02$right_profile_index), as.numeric(tester002$right_profile_index))
  expect_equal(as.numeric(tester0100$right_profile_index), as.numeric(tester00100$right_profile_index))
  expect_equal(as.numeric(tester01$right_matrix_profile), as.numeric(tester001$right_matrix_profile))
  expect_equal(as.numeric(tester02$right_matrix_profile), as.numeric(tester002$right_matrix_profile))
  expect_equal(as.numeric(tester0100$right_matrix_profile), as.numeric(tester00100$right_matrix_profile))
})

test_that("Normal mpx + update batch right new", {
  tester00100 <- mpxi_rcpp(datar0100[1001:1100], tester0, 0, FALSE) # 93050

  expect_equal(as.numeric(tester0100$right_profile_index), as.numeric(tester00100$right_profile_index))
  expect_equal(as.numeric(tester0100$right_matrix_profile), as.numeric(tester00100$right_matrix_profile))
})


test_that("Normal mpx + increment right", {
  testerr00100 <- tester0
  for (i in 1:100) {
    testerr00100 <- mpxiright_rcpp(datar0100[1:(1100 - 100 + i)], 80, 0.5, 1.0, TRUE, FALSE, FALSE, 1, testerr00100)
  }

  expect_equal(as.numeric(tester0100$right_profile_index), as.numeric(testerr00100$right_profile_index))
  expect_equal(as.numeric(tester0100$right_matrix_profile), as.numeric(testerr00100$right_matrix_profile))
})

test_that("Normal mpx + increment right new", {
  testerr00100 <- tester0
  for (i in 1:100) {
    testerr00100 <- mpxi_rcpp(datar0100[1000 + i], testerr00100, 0, FALSE) # 93050
  }

  expect_equal(as.numeric(tester0100$right_profile_index), as.numeric(testerr00100$right_profile_index))
  expect_equal(as.numeric(tester0100$right_matrix_profile), as.numeric(testerr00100$right_matrix_profile))
})

test_that("Normal mpx + update batch left", {
  testel001 <- mpxileft_rcpp(datal01, 80, 0.5, 1.0, TRUE, FALSE, FALSE, 1, testel0) # 881
  testel002 <- mpxileft_rcpp(datal02, 80, 0.5, 1.0, TRUE, FALSE, FALSE, 2, testel0) # 1763
  testel00100 <- mpxileft_rcpp(datal0100, 80, 0.5, 1.0, TRUE, FALSE, FALSE, 100, testel0) # 93050

  expect_equal(as.numeric(testel01$left_profile_index), as.numeric(testel001$left_profile_index))
  expect_equal(as.numeric(testel02$left_profile_index), as.numeric(testel002$left_profile_index))
  expect_equal(as.numeric(testel0100$left_profile_index), as.numeric(testel00100$left_profile_index))
  expect_equal(as.numeric(testel01$left_matrix_profile), as.numeric(testel001$left_matrix_profile))
  expect_equal(as.numeric(testel02$left_matrix_profile), as.numeric(testel002$left_matrix_profile))
  expect_equal(as.numeric(testel0100$left_matrix_profile), as.numeric(testel00100$left_matrix_profile))
})

test_that("Normal mpx + increment left", {
  testell00100 <- testel0
  for (i in 1:100) {
    testell00100 <- mpxileft_rcpp(datal0100[(101 - i):1100], 80, 0.5, 1.0, TRUE, FALSE, FALSE, 1, testell00100)
  }

  expect_equal(as.numeric(testel0100$left_profile_index), as.numeric(testell00100$left_profile_index))
  expect_equal(as.numeric(testel0100$left_matrix_profile), as.numeric(testell00100$left_matrix_profile))
})
