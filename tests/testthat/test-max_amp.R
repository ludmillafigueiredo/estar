test_that("max_amp needs a matrix as input", {
  expect_error(max_amp(aquacomm_resps))
  expect_error(max_amp(aquacomm_fgps))
})
