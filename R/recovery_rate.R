#' Calculate the rate of recovery.
#'
#' @description Returns the rate of log-ratio response (LRR) of a state variable
#' in relation to a baseline, over a time frame. The slope can be calculated as
#' the slope of a linear model derived for the LRR over time or as the slope
#' between two time steps.
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
#' to a baseline time series (\code{bl_mode = "ts"}) or to a point in the disturbed
#' state variable time series (\code{bl_mode = "point"}). ##V: this argument is redundant with slope_mode, that one is enough for this function
#' @param t_rec if \code{bl_mode = "point"}, the time step at which state variable
#'  value should be used as the starting time point for recovery rate calculation.
#' @param svbl_i a numeric vector containing the state variable in the baseline,
#' or a string for the name of the column in \code{bl_data} containing said
#' variable in the baseline.
#' Obligatory argument if (\code{slope_mode = "bl"}).
#' @param tbl_i an optional numeric vector containing the time steps for which
#' the baseline was measured, or a string containing the name of the column in
#' \code{bl_data}.
#' Obligatory argument if (\code{slope_mode = "bl"}).  ## V: the slope_mode = 'bl' is not mentioned in the documentation
#' @param bl_data an optional data frame containing the time-series of the
#' baseline values of the state variable. Time and value columns must be named
#' \code{tbl_i} and \code{svbl_i}, respectively.
#' @param slope_mode a string stating whether recovery rate is calculated as the
#' slope of a linear model (\code{slope_mode = "lm"}) or between two points in
#' the time series (\code{slope_mode = "points"}).
#' @param slope_tf a vector containing the first and last time steps defining
#' the time frame between which the rate of recovery should be calculated.
#' @param na_rm a logical indicating whether NA values should be removed before
#' processing.
#'
#' @return a double, the rate of recovery
#'
#' @examples
#' recovery_rate(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_svts, bl_mode = "ts",
#'   svbl_i = "stat_var", tbl_i = "time", bl_data = toy_blts,
#'   slope_mode = "lm", slope_tf = c(12, 50)
#' )
#' recovery_rate(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_svts, bl_mode = "point",
#'   t_rec = 9, svbl_i = "stat_var", tbl_i = "time", bl_data = toy_blts,
#'   slope_mode = "lm", slope_tf = c(12, 50)
#' )
#' recovery_rate(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_svts, bl_mode = "ts",
#'   svbl_i = "stat_var", tbl_i = "time", bl_data = toy_blts,
#'   slope_mode = "points", slope_tf = c(12, 50)
#' )
#' recovery_rate(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_svts, bl_mode = "point",
#'   t_rec = 9, svbl_i = "stat_var", tbl_i = "time", bl_data = toy_blts,
#'   slope_mode = "points", slope_tf = c(12, 50)
#' )
#' @export
recovery_rate <- function(svdb_i, tdb_i, db_data, bl_mode, t_rec = NULL, svbl_i = NULL,
                          tbl_i = NULL, bl_data = NULL, slope_mode, slope_tf,
                          na_rm = TRUE) {
  dbts_df <- format_input(input = "db", svdb_i, tdb_i, db_data)
  if (bl_mode == "ts") {
    blts_df <- format_input(input = "bl", svbl_i, tbl_i, bl_data)
    base_df <- dplyr::left_join(
      dplyr::rename(dbts_df, "t" = tdb_c),
      dplyr::rename(blts_df, "t" = tbl_c)
    ) %>%
      dplyr::mutate(extent = log(svdb_c / svbl_c)) %>%
      dplyr::select(t, extent)
  } else {
    if (bl_mode == "point") {
      svbl_p <- dplyr::filter(dbts_df, tdb_c == t_rec) %>%
        dplyr::pull(svdb_c)
      warning("You are using a single point as baseline.")
      base_df <- dbts_df %>%
        dplyr::mutate(extent = log(svdb_c / svbl_p)) %>%
        dplyr::rename("t" = tdb_c)
    } else {
      stop("bl_mode must be 'ts' or 'point'.")
    }
  }

  if (slope_mode == "lm") {
    lm_df <- base_df %>%
      dplyr::filter(t >= min(slope_tf), t <= max(slope_tf))

    rate_lm <- stats::lm(extent ~ t, data = lm_df)

    return(rate_lm$coefficients[["t"]])
  } else {
    if (slope_mode == "points") {
      rate_df <- base_df %>%
        dplyr::filter(t == min(slope_tf) | t == max(slope_tf)) %>%
        dplyr::arrange(t) %>%
        dplyr::mutate(lim = dplyr::case_when(
          t == min(slope_tf, na.rm = na_rm) ~ "min_t",
          t == max(slope_tf, na.rm = na_rm) ~ "max_t"
        )) %>%
        dplyr::select(lim, extent) %>%
        tidyr::pivot_wider(names_from = lim, values_from = extent) %>%
        dplyr::mutate(rate = (max_t - min_t) / (max(slope_tf) - min(slope_tf)))

      return(rate_df$rate)
    } else {
      stop("slope_mode must be 'lm' or 'points'.")
    }
  }
}
