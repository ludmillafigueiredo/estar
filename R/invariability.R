#' Calculate the invariability of a state variable after disturbance.
#'
#' \code{invariability} returns the temporal invariability.
#' Invariability can be calculated using the post-disturbance values of the
#' state variable in the disturbed system, or the log-response ratio of the
#' state variable in the disturbed system compared to the baseline.
#' Two variants of invariability can be calculated: 1. the first one is calculated
#' as the inverse of the coefficient of variation of the state variable; 2. the
#' second one is calculated as the inverse of the standard deviation of
#' residuals of the linear model that uses the time as the predictor of the
#' state variable.
#'
#' @param mode A string stating which variant of invariability should be calculated,
#' the one based on the coefficient of variation of the state variable \code{mode = "cv"},
#' or the on ebased on fitting the linear model \code{"lm_res"}.
#' @inheritParams univar_params
#'
#' @return a numeric, the invariability value.
#'
#' @examples
#' invariability(
#'   vd_i = "statvar_db", td_i = "time", response = "sv", mode = "cv",
#'   metric_tf = c(11, 50), d_data = aquacomm_resps
#' )
#' invariability(
#'   vd_i = aquacomm_resps$statvar_db, td_i = aquacomm_resps$time, response = "sv",
#'   mode = "cv", metric_tf = c(11, 50)
#' )
#' invariability(
#'   vd_i = "statvar_db", td_i = "time", response = "lrr", mode = "lm_res",
#'   metric_tf = c(11, 50), d_data = aquacomm_resps, vb_i = "statvar_bl",
#'   tb_i = "time", b_data = aquacomm_resps
#' )
#' invariability(
#'   vd_i = aquacomm_resps$statvar_db, td_i = aquacomm_resps$time, response = "lrr",
#'   metric_tf = c(11, 50), mode = "lm_res", vb_i = aquacomm_resps$statvar_bl,
#'   tb_i = aquacomm_resps$time
#' )
#' @export
invariability <- function(vd_i, td_i, mode, metric_tf, d_data = NULL, response,
                          vb_i = NULL, tb_i = NULL, b_data = NULL, na_rm = TRUE) {

  dts_df <- format_input("d", vd_i, td_i, d_data)

  invar_df <- eStar::sort_response(response, dts_df, vb_i, tb_i, b_data)
  invar_df <- invar_df[invar_df$t >= min(metric_tf) & invar_df$t <= max(metric_tf), ]

  if (any(is.na(invar_df$response))) {
    warning("NAs detected among the entries of the state variable")

    if (sum(!is.na(invar_df$response)) < 10) {
      warning("Less than 10 data points are available for measuring invariability.")
    }
  }

  if (mode == "cv") {
    invar <- 1 / eStar::cv(invar_df$response, na_rm = na_rm)
    return(invar)
  } else if (mode == "lm_res") {
    invar <- 1 / stats::sd(stats::lm(invar_df$response ~ invar_df$t)$residuals)
    return(invar)
  } else {
    stop("'mode' argument must be \"cv\" or \"lm_res\"")
  }
}
