#' Organize the response variable upon which the statbility metric will be calculated
#'
#' @param response a string stating whether the values of the state variable in
#' the disturbed scenario (\code{response == "sv"}) or its log ratio in relation
#' to baseline should be taken as the response used to calculate the metric.
#' @param dbts_df the internal dataframe composed from the inputs regarding the
#' disturbed scenario
#' @param svbl_i a numeric vector containing the state variable in the baseline,
#' a string for the name of the column in \code{bl_data} containing said
#' variable in the baseline, or default \code{NULL}, if a separate baseline is not
#' being used.
#' @param tbl_i an optional numeric vector containing the time steps for which
#' the baseline was measured, or a string containing the name of the column in
#' \code{bl_data}.
#' @param bl_data an optional data frame containing the time-series of the
#' baseline values of the state variable. Time and value columns must be named
#' \code{tbl_i} and \code{svbl_i}, respectively.
#'
#' @inheritParams univar_params
#'
#' @noRd
#' @export
sort_response <- function(response, dbts_df, svbl_i, tbl_i, bl_data) {
  if (response == "sv") {
    response_df <- setNames(dbts_df, c("response", "t"))
  } else if (response == "lrr") {
    blts_df <- format_input("bl", svbl_i, tbl_i, bl_data)

    names(dbts_df)[names(dbts_df) == "tdb_i"] <- "t"
    names(blts_df)[names(blts_df) == "tbl_i"] <- "t"

    response_df <- merge(dbts_df, blts_df)
    response_df$response <- log(response_df$svdb_i / response_df$svbl_i)
  } else {
    stop("'response' argument must be \"sv\" or \"lrr\".")
  }

  return(response_df)
}
