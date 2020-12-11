#' Define parameters that are common to all functions
#' 
#' @param svdb_i a numeric vector containing the state variable in the
#' disturbed system or a string specifying the name of the column
#' containing said variable in the dataframe provided in \code{db_data}.
#' @param tdb_i a numeric vector containing the time or a string specifying the
#' name of the column containing the time in the dataframe provided
#' in \code{db_data}.
#' @param db_data an optional data frame containing the time-series of the values
#' of the state variable in a state considered to be disturbed.
#' @param svbl_i a numeric vector containing the state variable in the baseline,
#' or a string for the name of the column in \code{bl_data} containing said
#' variable in the baseline.
#' Obligatory argument if \code{bl = "bl"}.
#' @param tbl_i an optional numeric vector containing the time steps for which
#' the baseline was measured, or a string containing the name of the column in
#' \code{bl_data}.
#' Obligatory argument if \code{bl = "bl"}.
#' @param bl_data an optional data frame containing the columns storing the baseline
#' of the state variable.
#' @param bl a string stating whether the baseline is defined by a separate
#' baseline that is input (\code{bl = "input"}) or by a time period of
#' the disturbed system (\code{bl = "db"}), to be defined by \code{bl_tf}.
#' @param bl_tf a numerical vector, specifying the beginning and end of the
#' time interval of the time-series of the disturbed system that defines the
#' baseline. Obligatory if (\code{bl = "db"}), see 'Details'.
#' @param summ_mode A string, stating whether the baseline should be summarized as
#' the mean (\code{summ_mode = "mean"}) or the median (\code{summ_mode = "mean"}).
#' Defaults to "mean".
#' @param na_rm a logical determining whether NAs should be taken out for the
#' estimation of variation. Defaults to TRUE.
#' @param response a string stating whether invariability should be calculated
#' from the log-ratio response between the values in the disturbed scenario and
#' the baseline (\code{response = "lrr"}) or for the values in the disturbed
#' scenario alone.
#' @param metric_tf a numerical vector, specifying the beginning and end of the
#' time interval for which the metric should be measured.
common_parameters <- function(svdb_i,
                              tdb_i,
                              db_data,
                              svbl_i,
                              tbl_i,
                              bl_data,
                              bl,
                              bl_tf,
                              summ_mode,
                              na_rm,
                              response,
                              metric_df,
                              ...){
    return(invisible(NULL))
}
