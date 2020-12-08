#' Calculate the rate of recovery.
#'
#' @description Returns the rate of log-ratio response (LRR) of a state variable
#' in relation to a baseline, over a time frame. The slope can be calculated as
#' the slope of a linear model derived for the LRR over time or as the slope
#' between two time steps.  ## V: here docs has to be made crystal clear, happy to work on this
#'
#' @param svdb_i a numeric vector containing the state variable in the
#' disturbed system or a string specifying the name of the column
#' containing said variable in the dataframe provided in \code{db_data}.
#' @param tdb_i a numeric vector containing the time or a string specifying the
#' name of the column containing the time in the dataframe provided
#' in \code{db_data}.
#' @param db_data an optional data frame containing the time-series of the values
#' of the state variable in a state considered to be disturbed.
#' @param bl_mode a string determining whether recovery is calculated for the
#' time series of values in a disturbed system (\code{bl_mode = "db"}) or to the
#' log-ratio response in relation to the baseline (\code{bl_mode = "bl"}).
#' @param rec_tf a vector containing the first and last time steps defining
#' the time frame between which the rate of recovery should be calculated.
#' @param svbl_i a numeric vector containing the state variable in the baseline,
#' or a string for the name of the column in \code{bl_data} containing said
#' variable in the baseline.
#' Obligatory argument if (\code{bl_mode = "bl"}).
#' @param tbl_i an optional numeric vector containing the time steps for which
#' the baseline was measured, or a string containing the name of the column in
#' \code{bl_data}.
#' Obligatory argument if (\code{bl_mode = "bl"}).
#' @param bl_data an optional data frame containing the time-series of the
#' baseline values of the state variable. Time and value columns must be named
#' \code{tbl_i} and \code{svbl_i}, respectively.
#' @param na_rm a logical indicating whether NA values should be removed before
#' processing.
#'
#' @return a double, the rate of recovery
#'
#' @examples
#' recovery_rate(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_dbts, bl_mode = "db",
#'   rec_tf = c(12, 50)
#' )
#' recovery_rate(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_dbts, bl_mode = "bl",
#'   rec_tf = c(12, 50), svbl_i = "stat_var", tbl_i = "time", bl_data = toy_blts
#' )
#' @export
recovery_rate <- function(svdb_i, tdb_i, db_data, bl_mode, rec_tf,
                          svbl_i = NULL, tbl_i = NULL, bl_data = NULL, na_rm = TRUE) {
  dbts_df <- format_input(input = "db", svdb_i, tdb_i, db_data)
  if (bl_mode == "bl") {
    blts_df <- format_input(input = "bl", svbl_i, tbl_i, bl_data)
    base_df <- dplyr::left_join(
      dplyr::rename(dbts_df, "t" = tdb_c),
      dplyr::rename(blts_df, "t" = tbl_c),
      by = "t"
    ) %>%
      dplyr::mutate(extent = log(svdb_c / svbl_c)) %>%
      dplyr::select(t, extent)
  } else {
    if (bl_mode == "db") {
      base_df <- dbts_df %>%
        dplyr::rename(
          "extent" = svdb_c,
          "t" = tdb_c
        )
    } else {
      stop("bl_mode must be 'ts' or 'point'.")  ## V: ts or db?
    }
  }
  lm_df <- base_df %>%
    dplyr::filter(t >= min(rec_tf), t <= max(rec_tf))

  rate_lm <- stats::lm(extent ~ t, data = lm_df)

  return(rate_lm$coefficients[["t"]])
}
