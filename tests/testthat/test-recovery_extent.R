test_that("extent of recovery is numeric", {
  expect_equal(is.numeric(
    recovery_extent(
      type = "temporal",
      response = "lrr",
      b = "input",
      t_rec = 42,
      vd_i = "statvar_db",
      td_i = "time",
      d_data = aquacomm_resps,
      vb_i = "statvar_bl",
      tb_i = "time",
      b_data = aquacomm_resps
    )
  ), TRUE)
})
