#' Calculate the persistance of a state variable in a user-defined limit
#'
#' @description Return the number of time steps the state variable remained
#' inside an upper or lower limit
#'
#' @param sv_resp a numeric vector containing the state variable in the
#' disturbed system or a string specifying the name of the column
#' containing said variable in the dataframe provided in \code{data_resp}.
#' @param t_resp a numeric vector containing the time or a string specifying the
#' name of the column containing the time in the dataframe provided
#' in \code{data_resp}.
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
#' @param tf_perst a numerical vector, specifying the time step during which
#' persistence should be measured.
#' @param data_resp an optional data frame containing the columns storing the
#' response state variable and time.
#' @param sv_bl a numeric vector containing the state variable in the baseline,
#' or a string for the name of the column in \code{data_bl} containing said
#' variable in the baseline.
#' Obligatory argument if \code{lim_mode = "bl"}.
#' @param t_bl an optional numeric vector containing the time steps for which
#' the baseline was measured, or a string containing the name of the column in
#' \code{data_bl}.
#' Obligatory argument if \code{lim_mode = "bl"}.
#' @param data_bl an optional data frame containing the columns storing the baseline
#' of the state variable.
#'
#' @return an integer, the duration of persistence
#'
#' @examples
#' persistence(
#'   sv_resp = "stat_var", t_resp = "time", lim_mode = "value",
#'   lim_ref = 70, lim_pos = "u", tf_perst = c(30, 60), data_resp = toy_svts
#' )
#' persistence(
#'   sv_resp = "stat_var", t_resp = "time", lim_mode = "bl",
#'   lim_ref = 0.9, lim_pos = "u", tf_perst = c(30, 60), data_resp = toy_svts,
#'   sv_bl = "stat_var", t_bl = "time", data_bl = toy_blts
#' )
#' @export
persistence <- function(sv_resp, t_resp, lim_mode, lim_ref, lim_pos, tf_perst,
                        data_resp = NULL, sv_bl = NULL, t_bl = NULL, data_bl = NULL) {
  if (is.null(data_resp)) {
    respts_df <- data.frame("svr_col" = sv_resp, "svr_col" = t_resp)
  } else {
    respts_df <- data_resp %>%
      dplyr::select(
        "svr_col" = dplyr::all_of(sv_resp),
        "tr_col" = dplyr::all_of(t_resp)
      )
  }

  choose_lim <- function(lim_pos, svr_col, lim_ref) {
    if (lim_pos == "u") {
      svr_col >= lim_ref
    } else {
      if (lim_pos == "l") {
        svr_col <= lim_ref
      } else {
        stop("lim_pos should be \"u\" or \"l\".")
      }
    }
  }
  if (lim_mode == "value") {
    persistence <- respts_df %>%
      dplyr::filter(tr_col >= min(tf_perst), tr_col <= max(tf_perst)) %>%
      dplyr::filter(choose_lim(lim_pos, svr_col, lim_ref)) %>%
      dplyr::summarize(duration = length(svr_col)) %>%
      dplyr::pull(duration)
    return(persistence)
  } else {
    if (lim_mode == "bl") {
      if (is.null(data_bl)) {
        blts_df <- data.frame("svbl_col" = sv_bl, "tbl_col" = t_bl)
      } else {
        blts_df <- data_bl %>%
          dplyr::select(
            "svbl_col" = dplyr::all_of(sv_bl),
            "tbl_col" = dplyr::all_of(t_bl)
          )
      }
      persistence <- dplyr::rename(respts_df, t = tr_col) %>%
        dplyr::left_join(.,
          dplyr::rename(blts_df, t = tbl_col),
          by = "t"
        ) %>%
        dplyr::filter(t >= min(tf_perst), t <= max(tf_perst)) %>%
        dplyr::mutate(lim_bl = lim_ref * svbl_col) %>%
        dplyr::filter(choose_lim(lim_pos, svr_col, lim_ref)) %>%
        dplyr::summarize(duration = length(svr_col)) %>%
        dplyr::pull(duration)
      return(persistence)
    } else {
      stop("lim_mode must be \"value\" or \"bl\"")
    }
  }
}
