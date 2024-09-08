test_that("init_resil needs a matrix as input", {
  expect_error(init_resil(aquacomm_resps))
  expect_error(init_resil(aquacomm_fgps))
})
