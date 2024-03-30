#' Calculate the extent of recovery
#'
#' \code{recovery_extent} calculates how close a state variable is to its
#' baseline value at a time point specified by the user (usually after recovery
#' has taken place). This distance can be calculated as the log-response ratio
#' between the values in the disturbed system and the baseline
#' or as the difference between the state variables in a disturbed
#' time-series and the baseline. The baseline can be
#' \itemize{
#' \item a separate baseline time-series, which is summarized as a mean or median
#' according to \code{summ_mode}
#' \item the mean or median of pre-disturbance values of the state variable
#' in the disturbed system over a period defined by \code{bl_tf}}
#'
#' @param response a string stating whether the stability metric should be calculated
#' using the log-response ratio between the values in the disturbed system and
#' the baseline (\code{response = "lrr"}) or using the state variable values in the
#' disturbed system alone.
#' @param bl a string stating whether the baseline is defined by a separate
#' baseline that is specified by the user (\code{bl = "input"}) or by a
#' time period of the disturbed system (\code{bl = "db"}), to be defined by \code{bl_tf}.
#' @param t_rec An integer, time point at which the extent of recovery should be
#' calculated.
#' @param bl_tf a numerical vector, specifying the beginning and end of the
#' pre-disturbance time period for the disturbed time-series that defines
#' the baseline. Obligatory if (\code{bl = "db"}), see 'Details'.
#' @param summ_mode A string, stating whether the baseline should be summarized as
#' the mean (\code{summ_mode = "mean"}) or the median (\code{summ_mode = "mean"}).
#' Defaults to "mean".
#'
#' @inheritParams univar_params
#'
#' @details Even though it is possible to use a single data value as baseline
#' (by passing a double to \code{bl_tf}), it is not recommended, because a
#' single value does not account for any variability in the system arising from,
#' for example, demographic or environmental stochasticity.
#'
#' @return a double, the extent of recovery
#'
#' @examples
#' recovery_extent(
#'   svdb_i = "statvar_db", tdb_i = "time", db_data = aquacomm_resps, response = "lrr",
#'   bl = "input", t_rec = 50, svbl_i = "statvar_bl", tbl_i = "time",
#'   bl_data = aquacomm_resps
#' )
#' recovery_extent(
#'   svdb_i = "statvar_db", tdb_i = "time", db_data = aquacomm_resps, response = "diff",
#'   bl = "input", t_rec = 50, svbl_i = "statvar_bl", tbl_i = "time",
#'   bl_data = aquacomm_resps
#' )
#' recovery_extent(
#'   svdb_i = "statvar_db", tdb_i = "time", db_data = aquacomm_resps, response = "lrr",
#'   bl = "db", t_rec = 50, bl_tf = 9
#' )
#' recovery_extent(
#'   svdb_i = "statvar_db", tdb_i = "time", db_data = aquacomm_resps, response = "lrr",
#'   bl = "db", t_rec = 50, bl_tf = c(5, 10)
#' )
#' recovery_extent(
#'   svdb_i = "statvar_db", tdb_i = "time", db_data = aquacomm_resps, response = "lrr",
#'   bl = "db", t_rec = 50, bl_tf = c(5, 10), summ_mode = "median"
#' )
#' @export
recovery_extent <- function(svdb_i, tdb_i, db_data, response, bl, t_rec,
                            svbl_i = NULL, tbl_i = NULL, bl_data = NULL,
                            bl_tf = NULL, summ_mode = "mean",
                            na_rm = TRUE) {
  dbts_df <- format_input(input = "db", svdb_i, tdb_i, db_data)
  if (bl == "input") {
    blts_df <- format_input(input = "bl", svbl_i, tbl_i, bl_data)
    extent_df <- dplyr::left_join(
      dplyr::rename(dbts_df, "t" = tdb_i),
      dplyr::rename(blts_df, "t" = tbl_i),
      by = c("t")
    ) %>%
      dplyr::filter(t == t_rec)
  } else {
    if (bl == "db") {
      if (min(bl_tf) == max(bl_tf)) {
        warning("You are using a single point as baseline. Consider an interval, Details.")
      }
      bl <- summ_db2bl(dbts_df, bl_tf, summ_mode, na_rm)

      extent_df <- dbts_df %>%
        dplyr::filter(tdb_i == t_rec) %>%
        dplyr::mutate(svbl_i = bl)
    } else {
      stop("bl must be \"input\", \"point\", or \"period\".")
    }
  }

  if (response == "lrr") {
    extent <- extent_df %>%
      dplyr::mutate(extent = log(svdb_i / svbl_i)) %>%
      dplyr::pull(extent)
  } else {
    if (response == "diff") {
      extent <- extent_df %>%
        dplyr::mutate(extent = svdb_i - svbl_i) %>%
        dplyr::pull(extent)
    } else {
      stop("response must be \"lrr\" or \"diff\"")
    }
  }

  return(extent)
}
