#' Calculate the extent of recovery.
#'
#' @description Returns the log-ratio response of a state variable in relation to a baseline or between two time steps.
#'
#' @param svdb_i a numeric vector containing the state variable in the
#' disturbed system or a string specifying the name of the column
#' containing said variable in the dataframe provided in \code{db_data}.
#' @param tdb_i a numeric vector containing the time or a string specifying the
#' name of the column containing the time in the dataframe provided
#' in \code{db_data}.
#' @param db_data an optional data frame containing the time-series of the values
#' of the state variable in a state considered to be disturbed.
#' @param bl_mode a string determining whether recovery is calculated in relation
#' to a baseline time series (\code{bl_mode = "ts"}), to a point in the state
#' variable time series (\code{bl_mode = "point"}), or (\code{bl_mode = "period"})
#' to a summary (mean or median, defined by \code{summ_mode}) of the values of
#' the state variable during a period defined by \code{tfsd_bl}.
#' @param t_rec an integer, time step at which extent of recovery should be
#' calculated.
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
#' @param svasbl_t an integer stating the time point of the state variable response
#' time-series that should be used as baseline.
#' Obligatory argument if \code{bl_mode = "point"}.
#' @param svasbl_tf a numerical vector defining the period over which the values of
#' the state variable response should be used to calculate the baseline.
#' Obligatory argument if \code{bl_mode = "period"}.
#' @param summ_mode a string, stating whether the baseline should be summarized as
#' the mean (\code{summ_mode = "mean"}) or the median (\code{summ_mode = "mean"}).
#' @param bl_narm a logical indicating whether NA values should be removed before
#' summarizing the values to be used as baseline.
#' Obligatory argument if \code{bl_mode = "period"}.
#'
#' @return a double, the extent of recovery
#'
#' @examples
#' recovery_extent(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_dbts, bl_mode = "ts",
#'   t_rec = 50, svbl_i = "stat_var", tbl_i = "time", bl_data = toy_blts
#' )
#' recovery_extent(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_dbts, bl_mode = "point",
#'   t_rec = 50, svasbl_t = 9
#' )
#' recovery_extent(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_dbts, bl_mode = "period",
#'   t_rec = 50, svasbl_tf = c(5, 10), summ_mode = "mean"
#' )
#' recovery_extent(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_dbts, bl_mode = "period",
#'   t_rec = 50, svasbl_tf = c(5, 10), summ_mode = "median"
#' )
#' @export
recovery_extent <- function(svdb_i, tdb_i, db_data, bl_mode, t_rec, svbl_i = NULL,
                            tbl_i = NULL, bl_data = NULL, svasbl_t = NULL, svasbl_tf = NULL,
                            summ_mode = NULL, bl_narm = TRUE) {
  dbts_df <- format_input(input = "db", svdb_i, tdb_i, db_data)
  if (bl_mode == "ts") {
    blts_df <- format_input(input = "bl", svbl_i, tbl_i, bl_data)
    extent_df <- dplyr::left_join(
      dplyr::rename(dbts_df, "t" = tdb_c),
      dplyr::rename(blts_df, "t" = tbl_c),
      by = c("t")
    ) %>%
      dplyr::filter(t == t_rec)
  } else {
    if (bl_mode == "point") {
      if (is.null(svasbl_t)) {
        stop("Missing svasbl_t argument.")
      }
      svbl_df <- dplyr::filter(dbts_df, tdb_c == svasbl_t) %>%
        dplyr::rename("svbl_c" = svdb_c)
      extent_df <- dbts_df %>%
        dplyr::filter(tdb_c == t_rec) %>%
        dplyr::mutate(svbl_c = svbl_df$svbl_c)
      warning("You are using a single point as baseline. Consider an interval.")
    } else {
      if (bl_mode == "period") {
        summ_f <- match.fun(summ_mode)
        svbl_df <- dplyr::filter(
          dbts_df,
          tdb_c >= svasbl_tf, tdb_c <= svasbl_tf
        ) %>%
          dplyr::summarize("svbl_c" = summ_f(svdb_c, na.rm = bl_narm))

        extent_df <- dbts_df %>%
          dplyr::filter(tdb_c == t_rec) %>%
          dplyr::mutate("svbl_c" = svbl_df$svbl_c)
      } else {
        stop("bl_mode must be 'ts', 'point', or 'period'.")
      }
    }
  }

  extent <- extent_df %>%
    dplyr::mutate(extent = log(svdb_c / svbl_c)) %>%
    dplyr::pull(extent)
  return(extent)
}
