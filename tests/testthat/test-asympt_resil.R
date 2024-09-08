test_that("asympt_resil needs a matrix as input", {
  expect_error(asympt_resil(aquacomm_resps))
  expect_error(asympt_resil(aquacomm_fgps))
})
