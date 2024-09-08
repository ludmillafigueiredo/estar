test_that("invariability is a numeric value", {
  expect_equal(is.numeric(
    invariability(
      response = "lrr",
      mode = "lm_res",
      metric_tf = c(11, 50),
      vd_i = "statvar_db",
      td_i = "time",
      d_data = aquacomm_resps,
      vb_i = "statvar_bl",
      tb_i = "time",
      b_data = aquacomm_resps
    )),
    TRUE)
})
