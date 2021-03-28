#' Calculate the resistance of a state variable to disturbance
#'
#' \code{resistance} returns the distance of a state variable to a baseline
#' value at a specified time point. The distance is calculated as the maximal
#' absolute difference between the state variables in the disturbed system and
#' the baseline, or as the maximal log response ratio between these state variables,
#' at a specified time point.
#' See details on how to specify the values.
#'
#' @param res_mode A string stating whether the resistance should be calculated
#' as the log response ratio of the state variable in the disturbed system
#' compared to the baseline (\code{res_mode = "lrr"}) or the difference
#' (\code{res_mode = "diff"}) between the values of these state variables. See details.
#' @param res_time A string stating whether resistance should be calculated at
#' a specific point in time (\code{res_time = "defined"}) or if it should be taken
#' as the maximal difference between the disturbed and baseline state variables
#' over a specified time period (\code{res_time = "max"}. Time point or the time
#' period are defined by \code{res_t} and \code{res_tf}, respectively. See details.
#' @param res_t An integer defining the time point when resistance should be
#' measured if \code{res_time = "defined"}).
#' @param res_tf A vector, specifying the time period for which the maximum
#' resistance should be looked for, if \code{bl = "input"}. ## V: but also if bl = 'db', right?
#' @inheritParams univar_params
#'
#' @details If resistance is calculated at a specific time point, it is
#' conventionally the first time point after the disturbance.
#'
#' Even though it is possible to use a single data value as baseline
#' (by passing a double to \code{bl_tf}), it is not recommended, because a
#' single value does not account for any variability in the system arising from,
#' for example, demographic or environmental stochasticity.
#'
#' @return A double, the resistance of the state variable to disturbance.
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
#'   bl_tf = 9, res_mode = "lrr", res_time = "defined", res_t = 11
#' )
#' resistance(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_dbts, bl = "db",
#'   bl_tf = 9, res_mode = "diff", res_time = "defined", res_t = 11
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
#'   res_mode = "lrr", bl_tf = 9, res_time = "max",
#'   res_tf = c(11, 50)
#' )
#' resistance(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_dbts, bl = "db",
#'   summ_mode = "median", res_mode = "lrr", bl_tf = 9, res_time = "max",
#'   res_tf = c(11, 50)
#' )
#' @export
resistance <- function(svdb_i, tdb_i, db_data = NULL, bl, summ_mode = "mean",
                       svbl_i = NULL, tbl_i = NULL, bl_data = NULL, bl_tf = NULL,
                       res_mode, res_time, res_t = NULL, res_tf = NULL,
                       na_rm = TRUE) {
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
      if (min(bl_tf) == max(bl_tf)) {
        warning("You are using a single time point as baseline. Consider a time period, see Details.")
      }
      bl <- summ_db2bl(dbts_df, bl_tf, summ_mode, na_rm)
      res_df <- dbts_df %>%
        dplyr::rename("t" = tdb_c) %>%
        dplyr::mutate(svbl_c = bl)
    } else {
      stop("bl must be \"input\" or \"db\".")
    }
  }

  if (res_time == "defined") {
    res <- res_df %>%
      dplyr::filter(t == res_t) %>%
      dplyr::mutate(res = get_res(svdb_c, svbl_c, res_mode))%>%
      dplyr::pull(res)
  } else {
    if (res_time == "max") {
      res <- res_df %>%
        dplyr::filter(t >= min(res_tf), t <= max(res_tf)) %>%
        dplyr::mutate(res = get_res(svdb_c, svbl_c, res_mode)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(abs(res) == max(abs(res), na.rm = na_rm)) %>%
        dplyr::pull(res)
    } else {
      stop("res_time must be \"defined\" or \"max\".")
    }
  }
  return(res)
}
