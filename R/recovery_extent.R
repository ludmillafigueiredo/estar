#' Calculate the extent of recovery
#'
#' \code{recovery_extent} calculates how close a state variable at a certain
#' time step is to aa baseline. This distance can be calculated as the
#' log-ratio or the difference between the state variable in a disturbed
#' time-series and the baseline. The baseline can be
#' \itemize{
#' \item a separate baseline time-series
#' \item a single pre-disturbance value of the state variable
#' \item the mean or median of pre-disturbance values of the state variable
#' over a period defined by \code{bl_tf}}
#'
#' @param rec_mode A string stating whether the resistance should be calculated
#' as the log-ratio response (\code{res_mode = "lrr"}) or the difference
#' (\code{res_mode = "diff"}). See details.
#' @param bl A string determining whether recovery is calculated in relation
#' to a baseline time series (\code{bl = "input"}), to a point in the state
#' variable time series (\code{bl = "point"}), or (\code{bl = "period"})
#' to a summary (mean or median, defined by \code{summ_mode}) of the values of
#' the state variable during a period defined by \code{bl_tf}.
#' @param t_rec An integer, time step at which extent of recovery should be
#' calculated.
#' @param bl_t An integer stating the time point of the state variable response
#' time-series that should be used as baseline.
#' Obligatory argument if \code{bl = "point"}.
#' @param bl_tf A numerical vector defining the period over which the values of
#' the state variable response should be used to calculate the baseline.
#' Obligatory argument if \code{bl = "period"}.
#' @param summ_mode A string, stating whether the baseline should be summarized as
#' the mean (\code{summ_mode = "mean"}) or the median (\code{summ_mode = "mean"}).
#' @inheritParams common_parameters
#' 
#' @return a double, the extent of recovery
#'
#' @examples
#' recovery_extent(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_dbts, rec_mode = "lrr",
#'   bl = "input", t_rec = 50, svbl_i = "stat_var", tbl_i = "time",
#'   bl_data = toy_blts
#' )
#' recovery_extent(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_dbts, rec_mode = "diff",
#'   bl = "input", t_rec = 50, svbl_i = "stat_var", tbl_i = "time",
#'   bl_data = toy_blts
#' )
#' recovery_extent(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_dbts, rec_mode = "lrr",
#'   bl = "point", t_rec = 50, bl_t = 9
#' )
#' recovery_extent(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_dbts, rec_mode = "lrr",
#'   bl = "period", t_rec = 50, bl_tf = c(5, 10), summ_mode = "mean"
#' )
#' recovery_extent(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_dbts, rec_mode = "lrr",
#'   bl = "period", t_rec = 50, bl_tf = c(5, 10), summ_mode = "median"
#' )
#' @export
recovery_extent <- function(svdb_i, tdb_i, db_data, rec_mode, bl, t_rec,
                            svbl_i = NULL, tbl_i = NULL, bl_data = NULL,
                            bl_t = NULL, bl_tf = NULL, summ_mode = NULL,
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
    if (bl == "point") {
      if (is.null(bl_t)) {
        stop("Missing bl_t argument.")
      }
      svbl_df <- dplyr::filter(dbts_df, tdb_c == bl_t) %>%
        dplyr::rename("svbl_c" = svdb_c)
      extent_df <- dbts_df %>%
        dplyr::filter(tdb_c == t_rec) %>%
        dplyr::mutate(svbl_c = svbl_df$svbl_c)
      warning("You are using a single point as baseline. Consider an interval.")
    } else {
      if (bl == "period") {
        summ_f <- match.fun(summ_mode)
        svbl_df <- dplyr::filter(dbts_df,
          tdb_c >= min(bl_tf), tdb_c <= max(bl_tf)
          ) %>%
          dplyr::ungroup() %>%
          dplyr::summarize("svbl_c" = summ_f(svdb_c, na.rm = na_rm))

        extent_df <- dbts_df %>%
          dplyr::filter(tdb_c == t_rec) %>%
          dplyr::mutate(svbl_c = svbl_df$svbl_c)
      } else {
        stop("bl must be \"input\", \"point\", or \"period\".")
      }
    }
  }

  if (rec_mode == "lrr"){
  extent <- extent_df %>%
    dplyr::mutate(extent = log(svdb_c / svbl_c)) %>%
    dplyr::pull(extent) 
  } else{
      if (rec_mode == "diff"){
  extent <- extent_df %>%
    dplyr::mutate(extent = svdb_c - svbl_c) %>%
    dplyr::pull(extent)
      } else {
          stop("rec_mode must be \"lrr\" or \"diff\"")
      }
  }
  
  return(extent)
}
