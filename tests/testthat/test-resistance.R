test_that("resistance is numeric", {
  expect_equal(is.numeric(
    resistance(
      type = "functional",
      b = "input",
      res_mode = "lrr",
      res_time = "defined",
      res_t = 12,
      vd_i = "statvar_db",
      td_i = "time",
      d_data = aquacomm_resps,
      vb_i = "statvar_bl",
      tb_i = "time",
      b_data = aquacomm_resps
    )
  ), TRUE)
})
