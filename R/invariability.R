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
#' @param tf_invar a numeric vector, specifying the beginning and end of the
#' interval of \code{svdb_i} values, from which invariability should be calculated,
#' or the specific time values defining this interval.
#' @param na_rm a logical indicating whether NA values should be removed before
#' processing, defaults to TRUE.
#' @param svbl_i a numeric vector containing the state variable in the baseline,
#' or a string for the name of the column in \code{bl_data} containing said
#' variable in the baseline.
#' Obligatory argument if \code{mode = "lm_res"}.
#' @param tbl_i an optional numeric vector containing the time steps for which
#' the baseline was measured, or a string containing the name of the column in
#' \code{bl_data}.
#' Obligatory argument if \code{mode = "lm_res"}.
#' @param bl_data an optional data frame containing the time-series of the
#' baseline values of the state variable. Time and value columns must be named
#' \code{tbl_i} and \code{svbl_i}, respectively.
#' @param blinvar_tf a numeric vector, specifying the beginning and end of the
#' interval of \code{svbl_i} values from which invariability should be calculated,
#' or the specific time values defining this interval, if a baseline is provided.
#' Obligatory argument if \code{mode = "lm_res"}.
#'
#' @return a numeric, the invariability value.
#'
#' @examples
#' invariability(
#'   svdb_i = "stat_var", tdb_i = "time", response = "sv", mode = "cv",
#'   tf_invar = c(11, 50), db_data = toy_dbts
#' )
#' invariability(
#'   svdb_i = toy_dbts$stat_var, tdb_i = toy_dbts$time, response = "sv",
#'   mode = "cv", tf_invar = c(11, 50)
#' )
#' invariability(
#'   svdb_i = "stat_var", tdb_i = "time", response = "lrr", mode = "lm_res",
#'   tf_invar = c(11, 50), db_data = toy_dbts, svbl_i = "stat_var",
#'   tbl_i = "time", blinvar_tf = c(11, 50), bl_data = toy_blts
#' )
#' invariability(
#'   svdb_i = toy_dbts$stat_var, tdb_i = toy_dbts$time, response = "lrr",
#'   tf_invar = c(11, 50), mode = "lm_res", svbl_i = toy_blts$stat_var,
#'   tbl_i = toy_blts$time, blinvar_tf = c(11, 50)
#' )
#' @details
#' Invariance can be calculated as the inverse of the coefficient of variation
#' (\code{mode = "cv"} or as the standard deviation of the residuals of the linear model
#' with the predictor being the time and the response being the state variable or the
#' log response ratio of the state variable and the baseline (\code{mode = "lm_res"}).
#' @export
invariability <- function(svdb_i, tdb_i, mode, tf_invar, db_data = NULL, response,
                          svbl_i = NULL, tbl_i = NULL, blinvar_tf = NULL, bl_data = NULL,
                          na_rm = TRUE) {
  dbts_df <- format_input(input = "db", svdb_i, tdb_i, db_data)

  dbts_df <- dbts_df %>%
    dplyr::filter(tdb_c >= min(tf_invar), tdb_c <= max(tf_invar))

  if (response == "sv") {
    invar_df <- dplyr::rename(dbts_df,
      "response" = svdb_c,
      "t" = tdb_c
    )
  } else {
    if (response == "lrr") {
      blts_df <- format_input(input = "bl", svbl_i, tbl_i, bl_data) %>%
        dplyr::filter(tbl_c >= min(blinvar_tf), tbl_c <= max(blinvar_tf))

      invar_df <- dplyr::inner_join(
        dplyr::rename(dbts_df, "t" = tdb_c),
        dplyr::rename(blts_df, "t" = tbl_c),
        by = "t"
      ) %>%
        dplyr::mutate("response" = log(svdb_c / svbl_c))
    } else {
      stop("'response' argument must be \"cv\" or \"lrr\".")
    }
  }

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
