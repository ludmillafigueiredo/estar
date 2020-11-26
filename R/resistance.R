#' Calculate the resistance of a state variable to disturbance
#'
#' @description Returns the value of the maximal absolute response of a state
#' variable to its value at an specified time. The response is calculated as
#' as the difference between disturbed and undisturbed states, or as the log-ratio
#' between. See details on how to specify the values.
#' 
#' @param sv_resp a numeric vector containing the state variable in the
#' disturbed system or a string specifying the name of the column
#' containing said variable in the dataframe provided in \code{data_resp}.
#' @param t_resp a numeric vector containing the time or a string specifying the
#' name of the column containing the time in the dataframe provided
#' in \code{data_resp}.
#' @param data_resp an optional data frame containing the columns storing the
#' response state variable and time.
#' @param res_mode a string stating whether the resistance from the disturbed time
#' state and the baseline should be calculated as the log-ratio response
#' (\code{res_mode = "lrr"}) or the absolute difference (\code{res_mode = "abs"}).
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
#' @details 
#'
#' @return a double, the resistance between state variable in disturbed and
#' undisturbed conditions.
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
resistance <- function(sv_resp, t_resp, data_resp = NULL, res_mode, bl_mode,
                       sv_bl = NULL, t_bl = NULL, data_bl = NULL, tresp_bl = NULL,
                       res_time, t_res = NULL, tf_res = NULL, na_rm = TRUE) {
    get_res <- function(sv_resp, sv_bl, res_mode){
        if (res_mode == "lrr") {
            res = log(sv_resp / sv_bl)
        } else {
            if (res_mode == "diff") {
                res = sv_resp - sv_bl
            } else {
                stop("res_mode must be \"lrr\" or \"diff\".")
        }
        }
    }
  respts_df <- format_input(input = "dtb", sv_resp, t_resp, data_resp)

  if (bl_mode == "ts") {
    blts_df <- format_input(input = "udtb", sv_resp, t_resp, data_resp)

    res_df <- dplyr::inner_join(dplyr::rename(respts_df, "t" = "t_bl"),
      dplyr::rename(blts_df, "t" = "t_resp"),
      by = c("t")
    )
  } else {
    if (bl_mode == "point") {
      bl <- respts_df %>%
        dplyr::filter(t_resp == tresp_bl) %>%
        dplyr::pull(sv_resp)

      res_df <- respts_df %>%
        dplyr::rename("t" = "t_resp") %>%
        dplyr::mutate(sv_bl = bl)
    } else {
      stop("bl must be \"separate\" or \"previous\".")
    }
  }

  if (res_time == "single") {
    res_df <- res_df %>%
      dplyr::filter(t == t_res) %>%
      dplyr::mutate(res = get_res(sv_resp, sv_bl, res_mode))

    return(res_df$lrr)
  } else {
    if (res_time == "time_frame") {
      res_df <- res_df %>%
        dplyr::filter(t >= min(tf_res), t <= max(tf_res)) %>%
        dplyr::mutate(res = get_res(sv_resp, sv_bl, res_mode)) %>%
        dplyr::summarize(max_res = max(abs(res), na.rm = na_rm))

      return(res_df$max_res)
    } else {
      stop("res_time must be \"defined\" or \"single\".")
    }
  }
}
