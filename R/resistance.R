#' Calculate the resistance of a state variable to disturbance
#'
#' \code{resistance} returns the distance of a state variable to a baseline
#' value at an specified time. The distance is calculated as the maximal
#' absolute difference between the disturbed state and the baseline, or as
#' the log response ratio between them, at an specified time step.
#' See details on how to specify the values.
#'
#' @param bl_t An integer, specifying the time step whose state variable value
#' should be used as baseline.
#' Obligatory if \code{bl = "db"}.
#' @param res_mode A string stating whether the resistance should be calculated
#' as the log-ratio response (\code{res_mode = "lrr"}) or the difference
#' (\code{res_mode = "diff"}). See details.
#' @param res_time A string stating whether resistance should be calculated at
#' an specific time step (\code{res_time = "defined"}) or if it should be taken
#' as the maximal value over a timeframe (\code{res_time = "max"}. Time steps
#' are defined by \code{res_t} and \code{res_tf}, respectively. See details.
#' @param res_t An integer defining the time step when resistance should be
#' measured if \code{res_time = "defined"}).
#' @param res_tf A vector, specifying the interval for which the maximum
#' resistance should be looked for, if \code{bl = "input"}.
#' @inheritParams common_parameters
#' 
#' @details If resistance is calculated at an specific time step, it is
#' traditionally the first time step following disturbance.
#'
#' @return A double, the resistance of the state variable to baseline.
#'
#' @examples
#' resistance(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_dbts, bl = "input",
#'   svbl_i = "stat_var", tbl_i = "time", bl_data = toy_blts,
#'   res_mode = "lrr", res_time = "defined", res_t = 11
#' )
#' resistance(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_dbts, bl = "input",
#'   svbl_i = "stat_var", tbl_i = "time", bl_data = toy_blts,
#'   res_mode = "diff", res_time = "defined", res_t = 11
#' )
#' resistance(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_dbts, bl = "db",
#'   bl_t = 9, res_mode = "lrr", res_time = "defined", res_t = 11
#' )
#' resistance(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_dbts, bl = "db",
#'   bl_t = 9, res_mode = "diff", res_time = "defined", res_t = 11
#' )
#' resistance(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_dbts, bl = "input",
#'   svbl_i = "stat_var", tbl_i = "time", bl_data = toy_blts,
#'   res_mode = "lrr", res_time = "max", res_tf = c(11, 50)
#' )
#' resistance(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_dbts, bl = "input",
#'   svbl_i = "stat_var", tbl_i = "time", bl_data = toy_blts,
#'   res_mode = "diff", res_time = "max", res_tf = c(11, 50)
#' )
#' resistance(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_dbts, bl = "db",
#'   res_mode = "lrr", bl_t = 9, res_time = "max", res_tf = c(11, 50)
#' )
#' resistance(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_dbts, bl = "db",
#'   res_mode = "diff", bl_t = 9, res_time = "max", res_tf = c(11, 50)
#' )
#' @export
resistance <- function(svdb_i, tdb_i, db_data = NULL, bl, svbl_i = NULL,
                       tbl_i = NULL, bl_data = NULL, bl_t = NULL, res_mode,
                       res_time, res_t = NULL, res_tf = NULL, na_rm = TRUE) {
  get_res <- function(svdb_c, svbl_c, res_mode) {
    if (res_mode == "lrr") {
      res <- log(svdb_c / svbl_c)
      return(res)
    } else {
      if (res_mode == "diff") {
        res <- svdb_c - svbl_c
        return(res)
      } else {
        stop("res_mode must be \"lrr\" or \"diff\".")
      }
    }
  }
  dbts_df <- format_input(input = "db", svdb_i, tdb_i, db_data)

  if (bl == "input") {
    blts_df <- format_input(input = "bl", svbl_i, tbl_i, bl_data)

    res_df <- dplyr::inner_join(dplyr::rename(dbts_df, "t" = tdb_c),
      dplyr::rename(blts_df, "t" = tbl_c),
      by = c("t")
    )
  } else {
    if (bl == "db") {
      bl <- dbts_df %>%
        dplyr::filter(tdb_c == bl_t) %>%
        dplyr::pull(svdb_c)

      res_df <- dbts_df %>%
        dplyr::rename("t" = tdb_c) %>%
        dplyr::mutate(svbl_c = bl)
    } else {
      stop("bl must be \"input\" or \"db\".")
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
      stop("res_time must be \"defined\" or \"max\".")
    }
  }
}
