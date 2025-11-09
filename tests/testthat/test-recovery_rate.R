test_that("rate of recovery is numeric", {
  expect_equal(is.numeric(
    recovery_rate(
      type = "functional",
      response = "v",
      b = "d",
      metric_tf = c(12, 50),
      b_tf = c(-4, 0),
      vd_i = "statvar_db",
      td_i = "time",
      d_data = aquacomm_resps
    )
  ), TRUE)
})
