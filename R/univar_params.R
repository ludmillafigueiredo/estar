#' Define parameters that are common to all functions
#'
#' @param svdb_i a numeric vector containing the state variable in the
#' disturbed system or a string specifying the name of the column
#' containing said variable in the dataframe provided in \code{db_data}.
#' @param tdb_i a numeric vector containing the time or a string specifying the
#' name of the column containing the time in the dataframe provided
#' in \code{db_data}.
#' @param db_data an optional data frame containing the time series of the
#' state variable values in a disturbed system.
#' @param svbl_i a numeric vector containing the state variable in the baseline,
#' or a string for the name of the column in \code{bl_data} containing said
#' variable in the dataframe with baseline values.
#' Obligatory argument if \code{bl = "bl"}.
#' @param tbl_i an optional numeric vector containing the time period over which
#' the baseline was measured, or a string containing the name of the column in
#' \code{bl_data}.
#' Obligatory argument if \code{bl = "bl"}.
#' @param bl_data an optional data frame containing the time series of the
#' state variable values in the baseline.
#' @param na_rm a logical determining whether NAs should be taken out prior to the
#' estimation of the stability metric. Defaults to TRUE.

univar_params <- function(svdb_i,
                              tdb_i,
                              db_data,
                              svbl_i,
                              tbl_i,
                              bl_data,
                              na_rm){
    return(invisible(NULL))
}
