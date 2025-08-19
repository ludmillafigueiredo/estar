#' Calculate the rate of recovery.
#'
#' \code{recovery_rate} returns the rate of recovery calculated as the slope of
#' a linear model which uses the time as a predictor of the response.
#' The response can be the state variable in a disturbed system, or the
#' log-response ratio (LRR) of the state variable in the disturbed system
#' compared to the baseline.
#'
#' @inheritParams univar_params
#'
#' @return a double, the rate of recovery
#'
#' @examples
#' recovery_rate(
#'   type = "functional", vd_i = "statvar_db", td_i = "time",
#'   d_data = aquacomm_resps, b = "d", metric_tf = c(12, 50)
#' )
#' recovery_rate(
#'   type = "functional", vd_i = "statvar_db", td_i = "time",
#'   d_data = aquacomm_resps, b = "input", metric_tf = c(12, 50),
#'   vb_i = "statvar_bl", tb_i = "time", b_data = aquacomm_resps
#' )
#' @export
recovery_rate <- function(type,
                          b = NULL,
                          metric_tf,
                          vd_i = NULL,
                          td_i = NULL,
                          d_data = NULL,
                          vb_i = NULL,
                          tb_i = NULL,
                          b_data = NULL,
                          comm_d = NULL,
                          comm_b = NULL,
                          comm_t = NULL,
                          method = "bray",
                          binary = "FALSE",
                          na_rm = TRUE) {
  dts_df <- format_input("d", vd_i, td_i, d_data)

  if (type == "functional"){
    if (b == "input") {
      bts_df <- format_input("b", vb_i, tb_i, b_data)

      base_df <- merge(
        data.frame("vd_i" = dts_df$vd_i, "t" = dts_df$td_i),
        data.frame("vb_i" = bts_df$vb_i, "t" = bts_df$tb_i),
        all.x = TRUE
      )
      base_df$extent = log(base_df$vd_i / base_df$vb_i)

    } else {
      if (b == "d") {
        base_df <- dts_df
        names(base_df)[names(base_df) == 'vd_i'] <- 'extent'
        names(base_df)[names(base_df) == 'td_i'] <- 't'
      } else {
        stop("b must be \"input\" or \"d\".")
      }
    }

    lm_df <- base_df[(base_df$t >= min(metric_tf) &
                        base_df$t <= max(metric_tf)),
                     c("t", "extent")]

  } else {

    base_df <- rbind(comm_d, comm_b) |>
      (\(.) .[.[[comm_t]] >= min(metric_tf) &
                .[[comm_t]] <= max(metric_tf), ])()

    dissim <- calc_dissim(base_df, comm_t, method, binary)

    lm_df <- data.frame(t = as.numeric(names(dissim)),
                        extent = unlist(dissim, use.names = FALSE))

  }

  rate_lm <- stats::lm(extent ~ t, data = lm_df)$coefficients[["t"]]

  return(rate_lm)
}
