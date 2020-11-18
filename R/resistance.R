#' Calculate the resistance of a state variable to disturbance
#'
#' @description Returns the log-ratio response of a state variable.
#' ## V: we should also calculate it as (classically done) the difference
#' ## between the state variable at the specified time point (usually right after disturbance)
#' ## and the baseline. You have all for it now, only need to add mode and then the
#' ## metric is just a difference, not lrr
#'
#' @param sv_resp a numeric vector containing the state variable in the
#' disturbed system or a string specifying the name of the column
#' containing said variable in the dataframe provided in \code{data_resp}.
#' @param t_resp a numeric vector containing the time or a string specifying the
#' name of the column containing the time in the dataframe provided
#' in \code{data_resp}.
#' @param data_resp an optional data frame containing the columns storing the
#' response state variable and time.
#' @param bl_mode a string stating the baseline in relation to which resistance
#' should be calculated. ## V: more details on the possible values here should be given
#' @param sv_bl a numeric vector containing the state variable in the baseline,
#' or a string for the name of the column in \code{data_bl} containing said
#' variable in the baseline.
#' @param t_bl an optional numeric vector containing the time steps for which
#' the baseline was measured, or a string containing the name of the column in
#' \code{data_bl}.
#' @param data_bl an optional data frame containing the columns storing the baseline
#' of the state variable.
#' @param tresp_bl an integer, specifying the time step that should be used
#' as baseline
#' @param res_time a string stating how to select the time step at which resistance
#' should be calculated.
#' @param t_res an integer defining the time step when resistance should be measured.
#' @param tf_res a vector, specifying the interval of \code{sv_resp} and
#' \code{sv_bl} values from which resistance should be calculated.
#' @param na_rm a logical indicating whether NA values should be removed before
#' processing.
#'
#' @return a double, the log-ratio response between \code{sv} and \code{sv_bl}.
#'
#' @examples
#' resistance(
#'   sv_resp = "stat_var", t_resp = "time", data_resp = toy_svts, bl_mode = "ts",
#'   sv_bl = "stat_var", t_bl = "time", data_bl = toy_blts,
#'   res_time = "single", t_res = 11
#' )
#' resistance(
#'   sv_resp = "stat_var", t_resp = "time", data_resp = toy_svts, bl_mode = "point",
#'   tresp_bl = 9, res_time = "single", t_res = 11
#' )
#' resistance(
#'   sv_resp = "stat_var", t_resp = "time", data_resp = toy_svts, bl_mode = "ts",
#'   sv_bl = "stat_var", t_bl = "time", data_bl = toy_blts,
#'   res_time = "time_frame", tf_res = c(11, 50)
#' )
#' resistance(
#'   sv_resp = "stat_var", t_resp = "time", data_resp = toy_svts, bl_mode = "point",
#'   tresp_bl = 9, res_time = "time_frame", tf_res = c(11, 50)
#' )
#' @export
resistance <- function(sv_resp, t_resp, data_resp = NULL, bl_mode, sv_bl = NULL, t_bl = NULL, data_bl = NULL,
                       tresp_bl = NULL, res_time, t_res = NULL, tf_res = NULL, na_rm = TRUE) {
  if (is.null(data_resp)) {
    respts_df <- data.frame("sv_resp" = sv_resp, "t_resp" = t_resp)
  } else {
    respts_df <- data_resp %>%
      dplyr::select(
        "sv_resp" = dplyr::all_of(sv_resp),
        "t_resp" = dplyr::all_of(t_resp)
      )
  }

  if (bl_mode == "ts") {
    if (is.null(data_bl)) {
      blts_df <- data.frame("sv_bl" = sv_bl, "t" = t_bl)
    } else {
      blts_df <- data_bl %>%
        dplyr::select(
          "sv_bl" = dplyr::all_of(sv_bl),
          "t_bl" = dplyr::all_of(t_bl)
        )
    }

    res_df <- dplyr::inner_join(dplyr::rename(respts_df),
      dplyr::rename(blts_df),
      by = c("t_resp" = "t_bl")
    ) %>%
      dplyr::rename("t" = t_resp)
  } else {
    if (bl_mode == "point") {
      bl <- respts_df %>%
        dplyr::filter(t_resp == tresp_bl) %>%
        dplyr::pull(sv_resp)

      res_df <- respts_df %>%
        dplyr::rename("t" = t_resp) %>%
        dplyr::mutate(sv_bl = bl)
    } else {
      stop("bl must be \"separate\" or \"previous\".")
    }
  }

  if (res_time == "single") {
    res_df <- res_df %>%
      dplyr::filter(t == t_res) %>%
      dplyr::mutate(lrr = log(sv_resp / sv_bl))

    return(res_df$lrr)
  } else {
    if (res_time == "time_frame") {
      res_df <- res_df %>%
        dplyr::filter(t %in% seq(tf_res)) %>%
        dplyr::mutate(lrr = log(sv_resp / sv_bl)) %>%
        dplyr::summarize(max_lrr = max(abs(lrr), na.rm = na_rm))

      return(res_df$max_lrr)
    } else {
      stop("res_time must be \"defined\" or \"single\".")
    }
  }
}
