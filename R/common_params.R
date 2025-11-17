#' Define parameters that are common to all functions
#'
#' @param type a string defining the type of stability (\code{"functional"} or \code{"compositional"}) to be calculated.
#' @param response a string stating whether the stability metric should be
#' calculated using the log-response ratio between the values in the disturbed
#' system and the baseline (\code{response = "lrr"}) or using the state
#' variable values in the disturbed system alone (\code{response == "v"}).
#' @param vd_i a numeric vector containing the state variable in the
#' disturbed system or a string specifying the name of the column
#' containing said variable in the dataframe provided in \code{d_data}.
#' @param td_i a numeric vector containing the time or a string specifying the
#' name of the column containing the time in the dataframe provided
#' in \code{d_data}.
#' @param d_data an optional data frame containing the time series of the
#' state variable values in a disturbed system.
#' @param vb_i an optional numeric vector containing the state variable in
#' the baseline, or a string for the name of the column in \code{b_data}
#' containing said variable in the dataframe with baseline values.
#' @param tb_i an optional numeric vector containing the time period over which
#' the baseline was measured, or a string for the name of the column in
#' \code{b_data} containing said the time variable in the dataframe
#' with baseline values.
#' @param b_data an optional data frame containing the time series of the
#' state variable values in the baseline.
#' @param b a string stating whether the baseline is defined by a separate
#' baseline that is specified by the user (\code{b = "input"}) or by a
#' period of the disturbed system (\code{b = "d"}) prior to the disturbance.
#' This period is specified by \code{b_tf}.
#' @param b_tf a numerical vector, specifying the beginning and end of the
#' pre-disturbance time period for the disturbed time-series that defines
#' the baseline. Obligatory if (\code{b = "d"}), see 'Details'.
#' @param na_rm a logical determining whether NAs should be taken out prior
#' to the estimation of the stability metric. Defaults to TRUE.
#' @param metric_tf a numerical vector, specifying the beginning and end of the
#' time period over which the stability metric should be measured.
#' @param comm_b a data frame containing long format community data (species
#' names as columns over time as rows) to calculate compositional metrics.
#' @param comm_d a data frame containing long format community data
#' (species as columns over time as rows) to calculate compositional metrics.
#' @param comm_t the name of the time variable in comm_b and comm_d.
#' @param method a string identifying the dissimilarity index to be used to
#' calculate dissimilarity. For more options, see \code{?vegdist}. 
#' Defaults to "bray".
#' @param binary a boolean stating whether presence/absence standardization
#' should be performed before calculating the dissimilarity. For more options, 
#' see \code{?vegdist}. Defaults to "bray".
common_params <- function(type,
                          response,
                          vd_i,
                          td_i,
                          d_data,
                          vb_i,
                          tb_i,
                          b_data,
                          b,
                          b_tf,
                          na_rm,
                          metric_tf,
                          comm_d,
                          comm_b,
                          comm_t,
                          method,
                          binary) {
  return(invisible(NULL))
}
