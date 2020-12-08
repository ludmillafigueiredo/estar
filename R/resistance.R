#' Calculate the resistance of a state variable to disturbance
#'
#' @description Returns the value of the maximal absolute response of a state
#' variable to its value at an specified time. The response is calculated as
#' as the difference between disturbed and undisturbed states, or as the log-ratio
#' between. See details on how to specify the values.
#'
#' @param svdb_v a numeric vector containing the state variable in the
#' disturbed system or a string specifying the name of the column
#' containing said variable in the dataframe provided in \code{db_data}.
#' @param tdb_v a numeric vector containing the time or a string specifying the
#' name of the column containing the time in the dataframe provided
#' in \code{db_data}.
#' @param db_data an optional data frame containing the time-series of the values
#' of the state variable in a state considered to be disturbed.
#' @param bl_mode a string stating the baseline is input as a time series
#' (\code{bl_mode = "ts"}), input by \code{svudb_v} and \code{tudb_v}), ## V: not sure these are up to date (svudb_v anbd tudb_v)
#' or should
#' taken from the disturbed time-series, at a time step to be specified by
#' \code{tsv_bl} (\code{bl_mode = "point"}).
#' @param svbl_v a numeric vector containing the values of the state variable to be
#' used the baseline, or a string for the name of the column in \code{data_udb}
#' containing these values.
#' Obligatory if \code{bl_mode = "ts"}.
#' @param tbl_v an optional numeric vector containing the time steps for which
#' the baseline was measured, or a string containing the name of the column in
#' \code{bl_data}.
#' Obligatory if \code{bl_mode = "ts"}.
#' @param bl_data an optional data frame containing the columns storing the values
#' of the state variable in a state considered to be undisturbed.
#' @param tsv_bl an integer, specifying the time step whose values should be used
#' as baseline.
#' Obligatory if \code{bl_mode = "point"}.
#' @param res_mode a string stating whether the resistance should be calculated
#' as the log-ratio response (\code{res_mode = "lrr"}) or the difference
#' (\code{res_mode = "diff"}). See details.
#' @param res_time a string stating whether resistance should be calculated at
#' an specific time step (\code{res_time = "defined"}) or if it should be taken
#' as the maximal value over a timeframe (\code{res_time = "max"}. Time steps
#' are defined by \code{res_t} and \code{res_tf}, respectively. See details.
#' @param res_t an integer defining the time step when resistance should be
#' measured if \code{res_time = "single"}).
#' @param res_tf a vector, specifying the interval for which the maximum resistance
#' should be looked for, if \code{bl_mode = "ts"}.
#' @param na_rm a logical indicating whether NA values should be removed before
#' processing.
#'
#' @details Whether \code{res_mode = "lrr"} or \code{res_mode = "diff"}, if
#' \code{res_time = "max"}, the resistance is value is selected as the maximal
#' absolute value.
#'
#' @return a double, the resistance of the state variable to baseline.
#'
#' @examples
#' resistance(
#'   svdb_v = "stat_var", tdb_v = "time", db_data = toy_dbts, bl_mode = "ts",
#'   svbl_v = "stat_var", tbl_v = "time", bl_data = toy_blts,
#'   res_mode = "lrr", res_time = "defined", res_t = 11
#' )
#' resistance(
#'   svdb_v = "stat_var", tdb_v = "time", db_data = toy_dbts, bl_mode = "ts",
#'   svbl_v = "stat_var", tbl_v = "time", bl_data = toy_blts,
#'   res_mode = "diff", res_time = "defined", res_t = 11
#' )
#' resistance(
#'   svdb_v = "stat_var", tdb_v = "time", db_data = toy_dbts, bl_mode = "point",
#'   tsv_bl = 9, res_mode = "lrr", res_time = "defined", res_t = 11
#' )
#' resistance(
#'   svdb_v = "stat_var", tdb_v = "time", db_data = toy_dbts, bl_mode = "point",
#'   tsv_bl = 9, res_mode = "diff", res_time = "defined", res_t = 11
#' )
#' resistance(
#'   svdb_v = "stat_var", tdb_v = "time", db_data = toy_dbts, bl_mode = "ts",
#'   svbl_v = "stat_var", tbl_v = "time", bl_data = toy_blts,
#'   res_mode = "lrr", res_time = "max", res_tf = c(11, 50)
#' )
#' resistance(
#'   svdb_v = "stat_var", tdb_v = "time", db_data = toy_dbts, bl_mode = "ts",
#'   svbl_v = "stat_var", tbl_v = "time", bl_data = toy_blts,
#'   res_mode = "diff", res_time = "max", res_tf = c(11, 50)
#' )
#' resistance(
#'   svdb_v = "stat_var", tdb_v = "time", db_data = toy_dbts, bl_mode = "point",
#'   res_mode = "lrr", tsv_bl = 9, res_time = "max", res_tf = c(11, 50)
#' )
#' resistance(
#'   svdb_v = "stat_var", tdb_v = "time", db_data = toy_dbts, bl_mode = "point",
#'   res_mode = "diff", tsv_bl = 9, res_time = "max", res_tf = c(11, 50)
#' )
#' @export
resistance <- function(svdb_v, tdb_v, db_data = NULL, bl_mode, svbl_v = NULL,
                       tbl_v = NULL, bl_data = NULL, tsv_bl = NULL, res_mode,
                       res_time, res_t = NULL, res_tf = NULL, na_rm = TRUE) {
  get_res <- function(svdb_v, svbl_v, res_mode) {
    if (res_mode == "lrr") {
      res <- log(svdb_v / svbl_v)
      return(res)
    } else {
      if (res_mode == "diff") {
        res <- svdb_v - svbl_v
        return(res)
      } else {
        stop("res_mode must be \"lrr\" or \"diff\".")
      }
    }
  }
  dbts_df <- format_input(input = "db", svdb_v, tdb_v, db_data)

  if (bl_mode == "ts") {
    blts_df <- format_input(input = "bl", svbl_v, tbl_v, bl_data)

    res_df <- dplyr::inner_join(dplyr::rename(dbts_df, "t" = tdb_c),
      dplyr::rename(blts_df, "t" = tbl_c),
      by = c("t")
    )
  } else {
    if (bl_mode == "point") {
      bl <- dbts_df %>%
        dplyr::filter(tdb_c == tsv_bl) %>%
        dplyr::pull(svdb_c)

      res_df <- dbts_df %>%
        dplyr::rename("t" = tdb_c) %>%
        dplyr::mutate(svbl_c = bl)
    } else {
      stop("bl_mode must be \"ts\" or \"point\".")
    }
  }

  if (res_time == "defined") {
    res_df <- res_df %>%
      dplyr::filter(t == res_t) %>%
      dplyr::mutate(res = get_res(svdb_c, svbl_c, res_mode))

    return(res_df$res)
  } else {
    if (res_time == "max") {
      res_df <- res_df %>%
        dplyr::filter(t >= min(res_tf), t <= max(res_tf)) %>%
        dplyr::mutate(res = get_res(svdb_c, svbl_c, res_mode)) %>%
        dplyr::filter(abs(res) == max(abs(res), na.rm = na_rm))

      return(res_df$res)
    } else {
      stop("res_time must be \"defined\" or \"single\".")  ## V: or 'max', not 'single', or?
    }
  }
}
