#' Calculate the extent of recovery
#'
#' \code{recovery_extent} calculates how close a state variable at a certain
#' time step is to aa baseline. This distance can be calculated as the
#' log-ratio or the difference between the state variable in a disturbed
#' time-series and the baseline. The baseline can be
#' \itemize{
#' \item a separate baseline time-series
#' \item the mean or median of pre-disturbance values of the state variable
#' over a period defined by \code{bl_tf}}
#'
#' @param rec_mode A string stating whether the resistance should be calculated
#' as the log-ratio response (\code{res_mode = "lrr"}) or the difference
#' (\code{res_mode = "diff"}). See details.
#' @param t_rec An integer, time step at which extent of recovery should be
#' calculated.
#' @inheritParams common_parameters
#'
#' @details Even though it is possible to use a single data value as baseline
#' (by passing a double to \code{bl_tf}), it is not recommended, because a
#' single value does not account for any variation on the system arising from
#' demographic or environmental dynamics or stochasticity.
#' 
#' @return a double, the extent of recovery
#'
#' @examples
#' recovery_extent(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_dbts, rec_mode = "lrr",
#'   bl = "input", bl_tf = 9, t_rec = 50, svbl_i = "stat_var", tbl_i = "time",
#'   bl_data = toy_blts
#' )
#' recovery_extent(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_dbts, rec_mode = "diff",
#'   bl = "input", bl_tf = 9, t_rec = 50, svbl_i = "stat_var", tbl_i = "time",
#'   bl_data = toy_blts
#' )
#' recovery_extent(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_dbts, rec_mode = "lrr",
#'   bl = "db", t_rec = 50, bl_tf = 9
#' )
#' recovery_extent(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_dbts, rec_mode = "lrr",
#'   bl = "db", t_rec = 50, bl_tf = c(5, 10)
#' )
#' recovery_extent(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_dbts, rec_mode = "lrr",
#'   bl = "db", t_rec = 50, bl_tf = c(5, 10), summ_mode = "median"
#' )
#' @export
recovery_extent <- function(svdb_i, tdb_i, db_data, rec_mode, bl, t_rec,
                            svbl_i = NULL, tbl_i = NULL, bl_data = NULL,
                            bl_tf = NULL, summ_mode = "mean",
                            na_rm = TRUE) {
  dbts_df <- format_input(input = "db", svdb_i, tdb_i, db_data)
  if (bl == "input") {
    blts_df <- format_input(input = "bl", svbl_i, tbl_i, bl_data)
    extent_df <- dplyr::left_join(
      dplyr::rename(dbts_df, "t" = tdb_c),
      dplyr::rename(blts_df, "t" = tbl_c),
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
        dplyr::filter(tdb_c == t_rec) %>%
        dplyr::mutate(svbl_c = bl)
    } else {
      stop("bl must be \"input\", \"point\", or \"period\".")
    }
  }

  if (rec_mode == "lrr") {
    extent <- extent_df %>%
      dplyr::mutate(extent = log(svdb_c / svbl_c)) %>%
      dplyr::pull(extent)
  } else {
    if (rec_mode == "diff") {
      extent <- extent_df %>%
        dplyr::mutate(extent = svdb_c - svbl_c) %>%
        dplyr::pull(extent)
    } else {
      stop("rec_mode must be \"lrr\" or \"diff\"")
    }
  }

  return(extent)
}
