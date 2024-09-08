test_that("reactivity needs a matrix as input", {
  expect_error(reactivity(aquacomm_resps))
  expect_error(reactivity(aquacomm_fgps))
})
