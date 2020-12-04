#' Calculate the persistance of a state variable in a user-defined limit
#'
#' @description Return the number of time steps the state variable remained
#' inside an upper or lower limit
#'
#' @param svdb_i a numeric vector containing the state variable in the
#' disturbed system or a string specifying the name of the column
#' containing said variable in the dataframe provided in \code{db_data}.
#' @param tdb_i a numeric vector containing the time or a string specifying the
#' name of the column containing the time in the dataframe provided
#' in \code{db_data}.
#' @param db_data an optional data frame containing the time-series of the values
#' of the state variable in a state considered to be disturbed.
#' @param lim_mode a string stating whether the limit is to be measured in
#' relation to a fixed value (\code{lim_mode = "value"}) or to a baseline
#' (\code{lim_mode = "bl"}).
#' The details of the two modes as explained in 'Details'.
#' @param lim_ref a double containing the fixed limit value
#' (if \code{lim_mode = "value"}) or the proportion of baseline to be used as
#' limit (if \code{lim_mode = "bl"}).
#' @param lim_pos a string stating whether an upper \code{limit = "u"} or lower
#' limit should be verified \code{limit = "l"}.
#' The details of the two modes as explained in 'Details'.
#' @param perst_tf a numerical vector, specifying the time step during which
#' persistence should be measured.
#' @param svbl_i a numeric vector containing the state variable in the baseline,
#' or a string for the name of the column in \code{bl_data} containing said
#' variable in the baseline.
#' Obligatory argument if \code{lim_mode = "bl"}.
#' @param tbl_i an optional numeric vector containing the time steps for which
#' the baseline was measured, or a string containing the name of the column in
#' \code{bl_data}.
#' Obligatory argument if \code{lim_mode = "bl"}.
#' @param bl_data an optional data frame containing the columns storing the baseline
#' of the state variable.
#'
#' @return an integer, the duration of persistence
#'
#' @examples
#' persistence(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_svts, lim_mode = "value",
#'   lim_ref = 70, lim_pos = "u", perst_tf = c(30, 60)
#' )
#' persistence(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_svts, lim_mode = "bl",
#'   lim_ref = 0.9, lim_pos = "u", perst_tf = c(30, 60),
#'   svbl_i = "stat_var", tdb_i = "time", bl_data = toy_blts
#' )
#' @export
persistence <- function(svdb_i, tdb_i, db_data = NULL, lim_mode, lim_ref, lim_pos,
                        perst_tf, svbl_i = NULL, tbl_i = NULL, bl_data = NULL) {
  choose_lim <- function(svr_c, lim_pos, lim_ref) {
    if (lim_pos == "u") {
      svr_c >= lim_ref
    } else {
      if (lim_pos == "l") {
        svr_c <= lim_ref
      } else {
        stop("lim_pos should be \"u\" or \"l\".")
      }
    }
  }
  dbts_df <- format_input(input = "db", svdb_i, tdb_i, db_data)
  if (lim_mode == "value") {
    persistence <- dbts_df %>%
      dplyr::filter(tr_c >= min(perst_tf), tr_c <= max(perst_tf)) %>%
      dplyr::filter(choose_lim(svr_c, lim_pos, lim_ref)) %>%
      dplyr::summarize(duration = length(svr_c)) %>%
      dplyr::pull(duration)
    return(persistence)
  } else {
    if (lim_mode == "bl") {
      blts_df <- format_input(input = "bl", svbl_i, tbl_i, bl_data)
      persistence <- dplyr::rename(dbts_df, t = tr_c) %>%
        dplyr::left_join(.,
          dplyr::rename(blts_df, t = tbl_c),
          by = "t"
        ) %>%
        dplyr::filter(t >= min(perst_tf), t <= max(perst_tf)) %>%
        dplyr::mutate(lim_bl = lim_ref * svbl_c) %>%
        dplyr::filter(choose_lim(svr_c, lim_pos, lim_ref)) %>%
        dplyr::summarize(duration = length(svr_c)) %>%
        dplyr::pull(duration)
      return(persistence)
    } else {
      stop("lim_mode must be \"value\" or \"bl\"")
    }
  }
}
