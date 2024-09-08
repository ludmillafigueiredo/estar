test_that("stoch_var needs a matrix as input", {
  expect_error(stoch_var(aquacomm_resps))
  expect_error(stoch_var(aquacomm_fgps))
})
