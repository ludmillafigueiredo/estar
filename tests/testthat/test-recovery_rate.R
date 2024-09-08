test_that("rate of recovery is numeric", {
  expect_equal(is.numeric(
    recovery_rate(
      b = "d",
      metric_tf = c(12, 50),
      vd_i = "statvar_db",
      td_i = "time",
      d_data = aquacomm_resps
    )
  ), TRUE)
})
