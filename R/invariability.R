#' Calculate the invariability of a state variable after disturbance.
#'
#' \code{invariability} returns the temporal invariability of the state variable
#' response to disturbance.
#' The response can be the post-disturbance values of the state variable, or the
#' log-response ratio of the state variable in the disturbed system compared to
#' the baseline.
#' The invariability itself can be calculated as the inverse of the coefficient
#' of variation of the state variable or as the inverse of the standard deviation of
#' residuals of the linear model that uses the time as the predictor of the
#' response.
#'
#' @param mode A string stating whether invariability should be calculated
#' from the coefficient of variation of the state variable \code{mode = "cv"},
#' or from the linear model \code{"lm_res"}.
#' The details of the two modes as explained in 'Details'.
#' @inheritParams common_parameters
#' 
#' @return a numeric, the invariability value.
#'
#' @examples
#' invariability(
#'   svdb_i = "stat_var", tdb_i = "time", response = "sv", mode = "cv",
#'   metric_tf = c(11, 50), db_data = toy_dbts
#' )
#' invariability(
#'   svdb_i = toy_dbts$stat_var, tdb_i = toy_dbts$time, response = "sv",
#'   mode = "cv", metric_tf = c(11, 50)
#' )
#' invariability(
#'   svdb_i = "stat_var", tdb_i = "time", response = "lrr", mode = "lm_res",
#'   metric_tf = c(11, 50), db_data = toy_dbts, svbl_i = "stat_var",
#'   tbl_i = "time", bl_data = toy_blts
#' )
#' invariability(
#'   svdb_i = toy_dbts$stat_var, tdb_i = toy_dbts$time, response = "lrr",
#'   metric_tf = c(11, 50), mode = "lm_res", svbl_i = toy_blts$stat_var,
#'   tbl_i = toy_blts$time
#' )
#' @export
invariability <- function(svdb_i, tdb_i, mode, metric_tf, db_data = NULL, response,
                          svbl_i = NULL, tbl_i = NULL, bl_data = NULL, na_rm = TRUE) {
  dbts_df <- format_input(input = "db", svdb_i, tdb_i, db_data)

  invar_df <- eStar::sort_response(response, dbts_df, svbl_i, tbl_i, bl_data) %>%
    dplyr::filter(t >= min(metric_tf), t <= max(metric_tf))

  if (any(is.na(invar_df$response))) {
    warning("NAs detected among the entries of the state variable")

    if (sum(!is.na(invar_df$response)) < 10) {
      warning("Less than 10 data points are available for measuring invariability.")
    }
  }

  if (mode == "cv") {
    invar <- 1 / eStar::cv(invar_df$response, na_rm = na_rm)

    return(invar)
  } else {
    if (mode == "lm_res") {
      invar <- 1 / stats::sd(stats::lm(invar_df$response ~ invar_df$t)$residuals)

      return(invar)
    } else {
      stop("'mode' argument must be \"cv\" or \"lm_res\"")
    }
  }
}
