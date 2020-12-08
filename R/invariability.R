#' Calculate the invariability of a state variable after disturbance.
#'
#' @description Return the temporal invariability of the state variable.
#' Can be aclulated as the inverse of coefficient of variation or
#' as the standard deviation of residuals of the linear model that uses
#' as the predictor  the time and as the response variable the log-response
#' ratio of the state variable in the disturbed system and in the baseline.
#'
#' @param svdb_i a numeric vector containing the state variable in the
#' disturbed system or a string specifying the name of the column
#' containing said variable in the dataframe provided in \code{db_data}.
#' @param tdb_i a numeric vector containing the time or a string specifying the
#' name of the column containing the time in the dataframe provided
#' in \code{db_data}.
#' @param db_data an optional data frame containing the time-series of the values
#' of the state variable in a state considered to be disturbed.
#' @param mode a string stating whether invariability should be calculated
#' from the coefficient of variation of the state variable \code{mode = "cv"},
#' or from the linear model between the response and time \code{"lm_res"}.
#' The details of the two modes as explained in 'Details'.
#' @param response a string stating whether invariability should be calculated
#' from the log-ratio response between the values in the disturbed scenario and
#' the baseline (\code{response = "lrr"}) or for the values in the disturbed
#' scenario alone.
#' @param invar_tf a numeric vector, specifying the beginning and end of the
#' interval of \code{svdb_i} values, from which invariability should be calculated,
#' or the specific time values defining this interval.
#' @param na_rm a logical indicating whether NA values should be removed before
#' processing, defaults to TRUE.
#' @param svbl_i a numeric vector containing the state variable in the baseline,
#' or a string for the name of the column in \code{bl_data} containing said
#' variable in the baseline.
#' Obligatory argument if \code{response = "lrr"}.
#' @param tbl_i an optional numeric vector containing the time steps for which
#' the baseline was measured, or a string containing the name of the column in
#' \code{bl_data}.
#' Obligatory argument if \code{response = "lrr"}.
#' @param bl_data an optional data frame containing the time-series of the
#' baseline values of the state variable. Time and value columns must be named
#' \code{tbl_i} and \code{svbl_i}, respectively.
#'
#' @return a numeric, the invariability value.
#'
#' @examples
#' invariability(
#'   svdb_i = "stat_var", tdb_i = "time", response = "sv", mode = "cv",
#'   invar_tf = c(11, 50), db_data = toy_dbts
#' )
#' invariability(
#'   svdb_i = toy_dbts$stat_var, tdb_i = toy_dbts$time, response = "sv",
#'   mode = "cv", invar_tf = c(11, 50)
#' )
#' invariability(
#'   svdb_i = "stat_var", tdb_i = "time", response = "lrr", mode = "lm_res",
#'   invar_tf = c(11, 50), db_data = toy_dbts, svbl_i = "stat_var",
#'   tbl_i = "time", bl_data = toy_blts
#' )
#' invariability(
#'   svdb_i = toy_dbts$stat_var, tdb_i = toy_dbts$time, response = "lrr",
#'   invar_tf = c(11, 50), mode = "lm_res", svbl_i = toy_blts$stat_var,
#'   tbl_i = toy_blts$time
#' )
#' @details
#' Invariance can be calculated as the inverse of the coefficient of variation
#' (\code{mode = "cv"} or as the standard deviation of the residuals of the linear model
#' with the predictor being the time and the response being the state variable or the
#' log response ratio of the state variable and the baseline (\code{mode = "lm_res"}).
#' @export
invariability <- function(svdb_i, tdb_i, mode, invar_tf, db_data = NULL, response,
                          svbl_i = NULL, tbl_i = NULL, bl_data = NULL, na_rm = TRUE) {
  dbts_df <- format_input(input = "db", svdb_i, tdb_i, db_data)

  invar_df <- eStar::sort_response(response, dbts_df, svbl_i, tbl_i, bl_data) %>%
    dplyr::filter(t >= min(invar_tf), t <= max(invar_tf))

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
