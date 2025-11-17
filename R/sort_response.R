#' Organize the response variable upon which the stability metric will be calculated
#'
#' @param response a string stating whether the values of the state variable in
#' the disturbed scenario (\code{response == "v"}) or its log ratio in relation
#' to baseline should be taken as the response used to calculate the metric.
#' @param dts_df the internal dataframe composed from the inputs regarding the
#' disturbed scenario
#' @param vb_i a numeric vector containing the state variable in the baseline,
#' a string for the name of the column in \code{b_data} containing said
#' variable in the baseline, or default \code{NULL}, if a separate baseline
#' is not being used.
#' @param tb_i an optional numeric vector containing the time steps for which
#' the baseline was measured, or a string containing the name of the column in
#' \code{b_data}.
#' @param b_data an optional data frame containing the time-series of the
#' baseline values of the state variable. Time and value columns must be named
#' \code{tb_i} and \code{vb_i}, respectively.
#'
#' @return A dataframe with the response variable calculated over time.
#'
#' @keywords internal
sort_response <- function(response, dts_df, vb_i, tb_i, b_data) {
  if (response == "v") {
    response_df <- stats::setNames(dts_df, c("response", "t"))
  } else if (response == "lrr") {
    bts_df <- format_input("b", vb_i, tb_i, b_data)

    response_df <- merge(
      data.frame("vd_i" = dts_df$vd_i, "t" = dts_df$td_i),
      data.frame("vb_i" = bts_df$vb_i, "t" = bts_df$tb_i)
    )
    response_df$response <- log(response_df$vd_i / response_df$vb_i)

  } else {
    stop("'response' argument must be \"v\" or \"lrr\".")
  }

  return(response_df)
}
