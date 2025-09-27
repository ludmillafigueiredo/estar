test_that("persistence is numeric", {
  expect_equal(is.numeric(
    persistence(
      type = "functional",
      b_tf = c(1, 9),
      metric_tf = c(28, 56),
      vd_i = "statvar_db",
      td_i = "time",
      d_data = aquacomm_resps,
      b = "d"
    )
  ), TRUE)
})
