# library(testthat)
# library(RcppDeepState)
# library(nc)

# package_path <- here::here()
# package_path <- gsub("/inst", "", package_path)
# result <- RcppDeepState::deepstate_pkg_create(package_path)

# test_that("deepstate create TestHarness", {
#   expect_equal(result, "Testharness created!!")
# })

# RcppDeepState::deepstate_harness_compile_run(package_path)

# test_that("log files check", {
#   error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/mass_weighted_rcpp/mass_weighted_log")
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/mass_absolute_rcpp/mass_absolute_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/mass2_rcpp/mass2_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/mass3_rcpp/mass3_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/mass3_parallel/mass3_parallel_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/set_k_rcpp/set_k_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/find_best_k_rcpp/find_best_k_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/mass_pre_rcpp/mass_pre_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/mass_pre_abs_rcpp/mass_pre_abs_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/mass_pre_weighted_rcpp/mass_pre_weighted_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/std_rcpp/std_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/list_to_matrix/list_to_matrix_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/mode_rcpp/mode_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/znorm_rcpp/znorm_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/normalize_rcpp/normalize_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/binary_split_rcpp/binary_split_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/ed_corr_rcpp/ed_corr_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/corr_ed_rcpp/corr_ed_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/inner_product/inner_product_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/sum_of_squares/sum_of_squares_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/fft_rcpp/fft_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/mpxi_rcpp/mpxi_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/mpxiright_rcpp/mpxiright_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/mpxileft_rcpp/mpxileft_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/mpx_rcpp/mpx_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/mpxab_rcpp/mpxab_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/mpx_parallel/mpx_parallel_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/mpxab_parallel/mpxab_parallel_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/scrimp_rcpp/scrimp_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/scrimp_parallel/scrimp_parallel_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/scrimpab_rcpp/scrimpab_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/stamp_rcpp/stamp_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/stamp_parallel/stamp_parallel_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/stomp_rcpp/stomp_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/stomp_parallel/stomp_parallel_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/movmean_rcpp/movmean_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/movstd_rcpp/movstd_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/movmean_std_rcpp/movmean_std_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/movvar_rcpp/movvar_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/movvar2_rcpp/movvar2_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/movsum_rcpp/movsum_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/movsum_ogita_rcpp/movsum_ogita_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/precision_test_rcpp/precision_test_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/movmin_rcpp/movmin_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/movmax_rcpp/movmax_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/movmean_weighted_rcpp/movmean_weighted_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/movmean_fading_rcpp/movmean_fading_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/movsum_weighted_rcpp/movsum_weighted_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/movsum_fading_rcpp/movsum_fading_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/movvar_weighted_rcpp/movvar_weighted_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/movvar_fading_rcpp/movvar_fading_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/muinvn_rcpp/muinvn_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/muinvn_parallel/muinvn_parallel_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })

# error_list <- RcppDeepState::deepstate_user_error_display("./inst/testfiles/zero_crossing_rcpp/zero_crossing_log")

# test_that("log files check", {
#   expect_equal(paste0(
#     error_list$src.file.lines,
#     collapse = ""
#   ), "")
# })
