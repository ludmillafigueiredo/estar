#' Calculate the resistance of a state variable to disturbance
#'
#' @description Returns the log-ratio response of a state variable.
#'
#' @param sv_resp a vector containing the response state variable or a string specifying
#' the name of the column containing said variable in the dataframe provided
#' in \code{data_resp}.
#' @param t_resp a vector containing the time or a string specifying the
#' name of the column containing the time in the dataframe provided
#' in \code{data_resp}.
#' @param data_resp an optional data frame containing the columns storing the
#' response state variable and time.
#' @param bl a string stating the baseline in relation to which resistance
#' should be calculated.
#' @param sv_bl a vector containing the baseline, or a string containing
#' the name of the column in \code{data_bl} containing the baseline.
#' Obligatory argument if \code{mode = "lm_res"}.
#' @param t_bl an optional vector containing the time steps for which the baseline
#' was measured, or a string containing the name of the column in \code{data_bl}.
#' Obligatory argument if \code{mode = "lm_res"}.
#' @param data_bl an optional data frame containing the columns storing the baseline
#' of the state variable.
#' @param tresp_bl an integer, specifying the time step that should be used
#' as baseline
#' @param res_time a string stating how to select the time step at which resistance
#' should be calculated.
#' @param t_res an integer defining the time step when resistance should be measured.
#' @param tf_res a vector, specifying the interval of \code{sv_resp} and
#' \code{sv_bl} values from which resistance should be calculated.
#'
#' @details
#' Read in a state variable time-series (\code{svts_df}) from the file path
#' \code{svts_path}.
#' If \code{bl = "time_series"}, read the baseline time-series from \code{bl_path},
#' otherwise, the baseline is the state variable at \code{t = t_bl}.
#' According to \code{res__time}, resistance can be calculated at the first
#' time step after disturbance (\code{t_d + 1}) or as the maximal deviation
#' from baseline inside a time frame defined by \code{time_frame}.
#'
#' @return a double, the log-ratio response between \code{sv} and \code{sv_bl}.
#'
#' @examples
#' resistance(
#'   sv_resp = "stat_var", t_resp = "time", data_resp = toy_svts, bl = "separate",
#'   sv_bl = "stat_var", t_bl = "time", data_bl = toy_blts,
#'   res_time = "single", t_res = 11
#' )
#' resistance(
#'   sv_resp = "stat_var", t_resp = "time", data_resp = toy_svts, bl = "previous",
#'   tresp_bl = 9, res_time = "single", t_res = 11
#' )
#' resistance(
#'   sv_resp = "stat_var", t_resp = "time", data_resp = toy_svts, bl = "separate",
#'   sv_bl = "stat_var", t_bl = "time", data_bl = toy_blts,
#'   res_time = "time_frame", tf_res = c(11, 50)
#' )
#' resistance(
#'   sv_resp = "stat_var", t_resp = "time", data_resp = toy_svts, bl = "previous",
#'   tresp_bl = 9, res_time = "time_frame", tf_res = c(11, 50)
#' )
#' @export
resistance <- function(sv_resp, t_resp, data_resp = NULL, bl, sv_bl = NULL, t_bl = NULL, data_bl = NULL,
                       tresp_bl = NULL, res_time, t_res = NULL, tf_res = NULL) {
  if (is.null(data_resp)) {
    respts_df <- data.frame("sv_resp" = sv_resp, "t_resp" = t_resp)
  } else {
    respts_df <- data_resp %>%
      dplyr::select(
        "sv_resp" = dplyr::all_of(sv_resp),
        "t_resp" = dplyr::all_of(t_resp)
      )
  }

  if (bl == "separate") {
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
    if (bl == "previous") {
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
        dplyr::summarize(max_lrr = max(lrr))

      return(res_df$max_lrr)
    } else {
      stop("res_time must be \"defined\" or \"single\".")
    }
  }
}
