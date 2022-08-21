skip_on_ci()
skip_on_cran()

datal0 <- rev(as.numeric(tsmp::mp_fluss_data$tilt_abp$data)[5001:6000])
datal01 <- rev(as.numeric(tsmp::mp_fluss_data$tilt_abp$data)[5001:6001])
datal02 <- rev(as.numeric(tsmp::mp_fluss_data$tilt_abp$data)[5001:6002])
datal0100 <- rev(as.numeric(tsmp::mp_fluss_data$tilt_abp$data)[5001:6100])
datar0 <- (as.numeric(tsmp::mp_fluss_data$tilt_abp$data)[5001:6000])
datar01 <- (as.numeric(tsmp::mp_fluss_data$tilt_abp$data)[5001:6001])
datar02 <- (as.numeric(tsmp::mp_fluss_data$tilt_abp$data)[5001:6002])
datar0100 <- (as.numeric(tsmp::mp_fluss_data$tilt_abp$data)[5001:6100])
tester0 <- mpx_stream_start(datar0, 80, 0.5, 0, FALSE)
tester01 <- mpx_stream_start(datar01, 80, 0.5, 0, FALSE)
tester02 <- mpx_stream_start(datar02, 80, 0.5, 0, FALSE)
tester0100 <- mpx_stream_start(datar0100, 80, 0.5, 0, FALSE)
testel0 <- mpx_stream_start(datal0, 80, 0.5, 0, FALSE)
testel01 <- mpx_stream_start(datal01, 80, 0.5, 0, FALSE)
testel02 <- mpx_stream_start(datal02, 80, 0.5, 0, FALSE)
testel0100 <- mpx_stream_start(datal0100, 80, 0.5, 0, FALSE)

test_that("Normal mpx + update batch right with external stats", {
  data <- as.numeric(tsmp::mp_fluss_data$tilt_abp$data)
  params <- list(
    window_size = 100,
    history = 5000,
    ez = 0.5,
    batch = 100,
    progress = FALSE
  )

  data_len <- length(data)
  profile_len <- params$history - params$window_size + 1

  msd <- muinvn(data, params$window_size)
  stats <- list(
    avg = msd$avg,
    sig = msd$sig,
    ddf = -1 * diff(data, lag = params$window_size) / 2,
    ddg = (data[seq.int(params$window_size + 1, data_len)] - msd$avg[seq.int(2, data_len - params$window_size + 1)]) +
      (data[seq.int(1, data_len - params$window_size)] - msd$avg[seq.int(1, data_len - params$window_size)])
  )

  data_vector <- seq.int(1, params$history)
  stats_vector <- seq.int(1, profile_len)

  partial_stats <- purrr::map(stats, function(x) x[stats_vector])
  partial_stats$ddf[profile_len] <- 0
  partial_stats$ddg[profile_len] <- 0

  initial_mp <- list(w = params$window_size, ez = params$ez, offset = 0)
  initial_mp <- mpx_stream_s_right(data[data_vector],
    batch_size = params$history, initial_mp, partial_stats, history = 0,
    mp_time_constraint = 0, progress = params$progress
  )
  test <- mpx_stream_start(data[data_vector], params$window_size, params$ez, 0, params$progress)

  expect_equal(as.numeric(initial_mp$right_matrix_profile), as.numeric(test$right_matrix_profile))
  expect_identical(as.numeric(initial_mp$right_profile_index), as.numeric(test$right_profile_index))
})

test_that("Normal mpx + update batch right with external stats2", {
  data <- as.numeric(tsmp::mp_fluss_data$tilt_abp$data)
  params <- list(
    window_size = 100,
    history = 5000,
    ez = 0.5,
    batch = 100,
    progress = FALSE
  )

  data_len <- length(data)
  profile_len <- params$history - params$window_size + 1

  msd <- muinvn(data, params$window_size)
  stats <- list(
    avg = msd$avg,
    sig = msd$sig,
    ddf = -1 * diff(data, lag = params$window_size) / 2,
    ddg = (data[seq.int(params$window_size + 1, data_len)] - msd$avg[seq.int(2, data_len - params$window_size + 1)]) +
      (data[seq.int(1, data_len - params$window_size)] - msd$avg[seq.int(1, data_len - params$window_size)])
  )

  data_vector <- seq.int(1, params$history)
  stats_vector <- seq.int(1, profile_len)

  partial_stats <- purrr::map(stats, function(x) x[stats_vector])
  partial_stats$ddf[profile_len] <- 0
  partial_stats$ddg[profile_len] <- 0

  initial_mp <- list(w = params$window_size, ez = params$ez, offset = 0)
  initial_mp <- mpx_stream_s_right(data[data_vector],
    batch_size = params$history, initial_mp, partial_stats, history = 0,
    mp_time_constraint = 0, progress = params$progress
  )
  test <- mpx_stream_start(data[data_vector], params$window_size, params$ez, 0, params$progress)

  expect_equal(as.numeric(test$right_matrix_profile), as.numeric(initial_mp$right_matrix_profile))
  expect_identical(as.numeric(test$right_profile_index), as.numeric(initial_mp$right_profile_index))
  expect_equal(initial_mp$offset, params$history)

  start <- initial_mp$offset - params$history + 1
  end <- initial_mp$offset + params$batch
  sec_data_vector <- seq.int(start, end)
  sec_profile_len <- length(sec_data_vector) - params$window_size + 1
  sec_stats_vector <- seq.int(start, start + sec_profile_len - 1)
  sec_stats <- purrr::map(stats, function(x) x[sec_stats_vector])
  sec_stats$ddf[sec_profile_len] <- 0
  sec_stats$ddg[sec_profile_len] <- 0
  sec_mp <- mpx_stream_s_right(data[sec_data_vector],
    batch_size = params$batch, initial_mp, sec_stats, params$history,
    mp_time_constraint = 0, params$progress
  )
  test_vector <- seq.int(start + params$batch, end)
  test <- mpx_stream_start(data[test_vector], params$window_size, params$ez, 0, params$progress)

  expect_equal(as.numeric(sec_mp$right_matrix_profile), as.numeric(test$right_matrix_profile))
  expect_identical(as.numeric(sec_mp$right_profile_index), as.numeric(test$right_profile_index))

  start <- sec_mp$offset - params$history + 1
  end <- sec_mp$offset + params$batch
  ter_data_vector <- seq.int(start, end)
  ter_profile_len <- length(ter_data_vector) - params$window_size + 1
  ter_stats_vector <- seq.int(start, start + ter_profile_len - 1)
  ter_stats <- purrr::map(stats, function(x) x[ter_stats_vector])
  ter_stats$ddf[ter_profile_len] <- 0
  ter_stats$ddg[ter_profile_len] <- 0
  ter_mp <- mpx_stream_s_right(data[ter_data_vector],
    batch_size = params$batch, sec_mp, ter_stats,
    params$history, mp_time_constraint = 0, params$progress
  )
  test_vector <- seq.int(start + params$batch, end)
  test <- mpx_stream_start(data[test_vector], params$window_size, params$ez, 0, params$progress)

  expect_equal(as.numeric(ter_mp$right_matrix_profile), as.numeric(test$right_matrix_profile))
  expect_identical(as.numeric(ter_mp$right_profile_index), as.numeric(test$right_profile_index))
})

test_that("Normal mpx + update batch right", {
  tester001 <- mpxiright_rcpp(datar01, 80, 0.5, 0, 1.0, TRUE, FALSE, FALSE, 1, tester0) # 881
  tester002 <- mpxiright_rcpp(datar02, 80, 0.5, 0, 1.0, TRUE, FALSE, FALSE, 2, tester0) # 1763
  tester00100 <- mpxiright_rcpp(datar0100, 80, 0.5, 0, 1.0, TRUE, FALSE, FALSE, 100, tester0) # 93050

  expect_identical(as.numeric(tester001$right_profile_index), as.numeric(tester01$right_profile_index))
  expect_identical(as.numeric(tester002$right_profile_index), as.numeric(tester02$right_profile_index))
  expect_identical(as.numeric(tester00100$right_profile_index), as.numeric(tester0100$right_profile_index))
  expect_equal(as.numeric(tester001$right_matrix_profile), as.numeric(tester01$right_matrix_profile))
  expect_equal(as.numeric(tester002$right_matrix_profile), as.numeric(tester02$right_matrix_profile))
  expect_equal(as.numeric(tester00100$right_matrix_profile), as.numeric(tester0100$right_matrix_profile))
})

test_that("Normal mpx + increment right", {
  testerr00100 <- tester0
  for (i in 1:100) {
    testerr00100 <- mpxiright_rcpp(datar0100[1:(1100 - 100 + i)], 80, 0.5, 0, 1.0, TRUE, FALSE, FALSE, 1, testerr00100)
  }

  expect_identical(as.numeric(tester0100$right_profile_index), as.numeric(testerr00100$right_profile_index))
  expect_equal(as.numeric(tester0100$right_matrix_profile), as.numeric(testerr00100$right_matrix_profile))
})


test_that("Normal mpx + update batch right new", {
  tester00100 <- mpxi_rcpp(datar0100[1001:1100], tester0, 0, 0, FALSE) # 93050

  expect_equal(as.numeric(tester00100$right_profile_index), as.numeric(tester0100$right_profile_index))
  expect_equal(as.numeric(tester00100$right_matrix_profile), as.numeric(tester0100$right_matrix_profile))
})

test_that("Normal mpx + update batch right new history", {
  data <- as.numeric(tsmp::mp_fluss_data$tilt_abp$data)
  params <- list(
    window_size = 100,
    history = 5000,
    ez = 0.5,
    batch = 100,
    progress = FALSE
  )

  idxs <- seq.int(1, params$history)
  idxs_new <- seq.int(params$history + 1, params$history + params$batch)

  initial_mp <- mpx_stream_start(data[idxs], params$window_size, params$ez, 0, FALSE)
  final_mp <- mpx_stream_start(data[(idxs + params$batch)], params$window_size, params$ez, 0, FALSE)
  test_mp <- mpxi_rcpp(data[idxs_new], initial_mp, params$history, 0, FALSE)

  expect_equal(as.numeric(test_mp$right_profile_index), as.numeric(final_mp$right_profile_index))
  expect_equal(as.numeric(test_mp$right_matrix_profile), as.numeric(final_mp$right_matrix_profile))
})

test_that("Normal mpx + increment right new", {
  testerr00100 <- tester0
  for (i in 1:100) {
    testerr00100 <- mpxi_rcpp(datar0100[1000 + i], testerr00100, 0, 0, FALSE) # 93050
  }

  expect_equal(as.numeric(testerr00100$right_profile_index), as.numeric(tester0100$right_profile_index))
  expect_equal(as.numeric(testerr00100$right_matrix_profile), as.numeric(tester0100$right_matrix_profile))
})

test_that("Normal mpx + update batch left", {
  testel001 <- mpxileft_rcpp(datal01, 80, 0.5, 1.0, TRUE, FALSE, FALSE, 1, testel0) # 881
  testel002 <- mpxileft_rcpp(datal02, 80, 0.5, 1.0, TRUE, FALSE, FALSE, 2, testel0) # 1763
  testel00100 <- mpxileft_rcpp(datal0100, 80, 0.5, 1.0, TRUE, FALSE, FALSE, 100, testel0) # 93050

  expect_identical(as.numeric(testel01$left_profile_index), as.numeric(testel001$left_profile_index))
  expect_identical(as.numeric(testel02$left_profile_index), as.numeric(testel002$left_profile_index))
  expect_identical(as.numeric(testel0100$left_profile_index), as.numeric(testel00100$left_profile_index))
  expect_equal(as.numeric(testel01$left_matrix_profile), as.numeric(testel001$left_matrix_profile))
  expect_equal(as.numeric(testel02$left_matrix_profile), as.numeric(testel002$left_matrix_profile))
  expect_equal(as.numeric(testel0100$left_matrix_profile), as.numeric(testel00100$left_matrix_profile))
})

test_that("Normal mpx + increment left", {
  testell00100 <- testel0
  for (i in 1:100) {
    testell00100 <- mpxileft_rcpp(datal0100[(101 - i):1100], 80, 0.5, 1.0, TRUE, FALSE, FALSE, 1, testell00100)
  }

  expect_identical(as.numeric(testel0100$left_profile_index), as.numeric(testell00100$left_profile_index))
  expect_equal(as.numeric(testel0100$left_matrix_profile), as.numeric(testell00100$left_matrix_profile))
})
