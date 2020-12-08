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
#' @param bl_data a numeric vector, specifying the beginning and end of the
#' interval of \code{svbl_i} values from which invariability should be calculated,
#' or the specific time values defining this interval, if a baseline is provided.
#' Obligatory argument if \code{mode = "lm_res"}.
#' @param blinvar_tf a numeric vector, specifying the beginning and end of the
#' interval of \code{svbl_i} values from which invariability should be calculated,
#' or the specific time values defining this interval, if a baseline is provided.
#' Obligatory argument if \code{mode = "lm_res"}.
#' @noRd
sort_response <- function(response, dbts_df, svbl_i, tbl_i, blinvar_tf, bl_data){
    if (response == "sv") {
    response_df <- dplyr::rename(dbts_df,
      "response" = svdb_c,
      "t" = tdb_c
    )
  } else {
    if (response == "lrr") {
      blts_df <- format_input(input = "bl", svbl_i, tbl_i, bl_data) %>%
        dplyr::filter(tbl_c >= min(blinvar_tf), tbl_c <= max(blinvar_tf))

      response_df <- dplyr::inner_join(
        dplyr::rename(dbts_df, "t" = tdb_c),
        dplyr::rename(blts_df, "t" = tbl_c),
        by = "t"
      ) %>%
        dplyr::mutate("response" = log(svdb_c / svbl_c))
    } else {
      stop("'response' argument must be \"cv\" or \"lrr\".")
    }
  }
    return(response_df)
}
