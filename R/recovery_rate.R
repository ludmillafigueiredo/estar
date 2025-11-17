#' Calculate the rate of recovery.
#'
#' \code{recovery_rate} ( \eqn{R_r} ) returns the rate of recovery calculated as
#' the slope of a linear model which uses the time as a predictor of the
#' response. The response can be the state variable in a disturbed system, the
#' log-response ratio or difference between the state variable in the disturbed
#' and baseline systems, or community dissimilarity.
#'
#' @param summ_mode A string, stating whether the baseline should be summarized
#' as the mean (\code{summ_mode = "mean"}) or the median
#' (\code{summ_mode = "median"}). Defaults to "mean".
#' @inheritParams common_params
#'
#' @return A numeric, the rate of recovery. If
#' \eqn{R_r = 0}
#' , the system did not react to the disturbance.
#' If
#' \eqn{R_r \ge 0}
#' , the system moved towards the values in the baseline after the disturbance
#' (recovery may be partial).
#' If
#' \eqn{Rr \le 0}
#' , the system deviated even further from the control.
#' In both cases, the higher
#' \eqn{R_r}
#' , the faster the response.
#'
#' @details
#' For functional stability, the response can be state variable
#' itself
#' ( \eqn{v_d} )
#' , or the log-response ratio or difference between the
#' state variable in the disturbed ( \eqn{v_d} ) and in the
#' baseline ( \eqn{v_b} or \eqn{v_p} if the baseline is pre-disturbance values).
#' For community stability, the response is the dissimilarity between the
#' disturbed ( \eqn{C_d} ) and baseline ( \eqn{C_b} ) communities. Therefore,
#'
#' \deqn{
#' R_r =
#' \frac{\sum (t - \bar{t})(y - \bar{y})}{
#'       \sum (t - \bar{t})^2},
#' \qquad
#' y \in \left\{
#'   v_d,\;
#'   \log\!\left(\frac{v_d}{v_b}\right),\;
#'   \log\!\left(\frac{v_d}{v_p}\right),\;
#'   v_d,\;
#'   \mathrm{dissim}\!\left(\frac{C_d}{C_b}\right)
#' \right\}
#' }
#'
#' @examples
#' recovery_rate(
#'   type = "functional", vd_i = "statvar_db", td_i = "time", response = "v",
#'   d_data = aquacomm_resps, b = "d", metric_tf = c(12, 50)
#' )
#' recovery_rate(
#'   type = "functional", vd_i = "statvar_db", td_i = "time", response = "v",
#'   d_data = aquacomm_resps, b = "input", metric_tf = c(12, 50),
#'   vb_i = "statvar_bl", tb_i = "time", b_data = aquacomm_resps
#' )
#' recovery_rate(
#'   type = "compositional", metric_tf = c(0.14, 28), comm_d = comm_dist,
#'   comm_b = comm_base, comm_t = "time"
#' )
#' @export
recovery_rate <- function(type,
                          b = NULL,
                          metric_tf,
                          response,
                          summ_mode = "mean",
                          b_tf = NULL,
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

    } else {
      if (b == "d") {
        base_df <- dts_df
        ## summarized baseline
        base_df$vb_i <- summ_d2b(dts_df, b_tf, summ_mode, na_rm)
        names(base_df)[names(base_df) == 'td_i'] <- 't'
      } else {
        stop("b must be \"input\" or \"d\".")
      }
    }

    if (response == "lrr"){
      base_df$extent = log(base_df$vd_i / base_df$vb_i)
    } else {
      base_df$extent = base_df$vd_i
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
