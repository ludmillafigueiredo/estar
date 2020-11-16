#' Calculate the rate of recovery.
#'
#' @description Returns the rate of log-ratio response (LRR) of a state variable
#' in relation to a baseline, over a time frame. The slope can be calculated as
#' the slope of a linear model derived for the LRR over time or as the slope
#' between two time steps.
#'
#' @param sv_resp a vector containing the response state variable or a string
#' specifying the name of the column containing said variable in the dataframe
#' provided in \code{data}.
#' @param t_resp a vector containing the time or a string specifying the name
#' of the column containing the time in the dataframe provided in \code{data}.
#' @param data_resp an optional data frame containing the columns storing the
#' response state variable and time.
#' @param bl_mode a string determining whether recovery is calculated in relation
#' to a baseline time series (\code{bl_mode = "ts"}) or to a point in the state
#' variable time series (\code{bl_mode = "point"}).
#' @param t_rec if \code{bl_mode = "point"}, the time step at which response value
#' should be used as the baseline.
#' @param sv_bl a vector containing the baseline, or a string containing
#' the name of the column in \code{data_bl} containing the baseline.
#' Obligatory argument if (\code{slope_mode = "bl"}).
#' @param t_bl an optional vector containing the time steps for which the baseline
#' was measured, or a string containing the name of the column in \code{data_bl}.
#' Obligatory argument if (\code{slope_mode = "bl"}).
#' @param data_bl an optional data frame containing the columns storing the
#' baseline of the state variable.
#' @param slope_mode a string stating whether recovery rate is calculated as the
#' slope of a linear model (\code{slope_mode = "lm"}) or between two points in
#' the time series (\code{slope_mode = "points"}).
#' @param tf_slope a vector containing the first and last time steps defining
#' the time frame between which the rate of recovery should be calculated.
#' @param na_rm a logical indicating whether NA values should be removed before
#' processing.
#'
#' @return a double, the rate of recovery
#'
#' @examples
#' recovery_rate(
#'   sv_resp = "stat_var", t_resp = "time", data_resp = toy_svts, bl_mode = "ts",
#'   t_rec = 50, sv_bl = "stat_var", t_bl = "time", data_bl = toy_blts,
#'   slope_mode = "lm", tf_slope = c(12, 50)
#' )
#' recovery_rate(
#'   sv_resp = "stat_var", t_resp = "time", data_resp = toy_svts, bl_mode = "point",
#'   t_rec = 9, sv_bl = "stat_var", t_bl = "time", data_bl = toy_blts,
#'   slope_mode = "lm", tf_slope = c(12, 50)
#' )
#' recovery_rate(
#'   sv_resp = "stat_var", t_resp = "time", data_resp = toy_svts, bl_mode = "ts",
#'   t_rec = 50, sv_bl = "stat_var", t_bl = "time", data_bl = toy_blts,
#'   slope_mode = "points", tf_slope = c(12, 50)
#' )
#' recovery_rate(
#'   sv_resp = "stat_var", t_resp = "time", data_resp = toy_svts, bl_mode = "point",
#'   t_rec = 9, sv_bl = "stat_var", t_bl = "time", data_bl = toy_blts,
#'   slope_mode = "points", tf_slope = c(12, 50)
#' )
#' @export
recovery_rate <- function(sv_resp, t_resp, data_resp, bl_mode, t_rec = NULL, sv_bl = NULL,
                          t_bl = NULL, data_bl = NULL, slope_mode, tf_slope,
                          na_rm = TRUE) {
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
      blts_df <- data.frame("sv_bl" = sv_bl, "t_bl" = t_bl)
    } else {
      blts_df <- data_bl %>%
        dplyr::select(
          "sv_bl" = dplyr::all_of(sv_bl),
          "t_bl" = dplyr::all_of(t_bl)
        )
    }
    base_df <- dplyr::left_join(
      dplyr::rename(respts_df, "t" = t_resp),
      dplyr::rename(blts_df, "t" = t_bl)
    ) %>%
      dplyr::mutate(extent = log(sv_resp / sv_bl)) %>%
      dplyr::select(t, extent)
  } else {
    if (bl_mode == "point") {
      sv_bl <- dplyr::filter(respts_df, t_resp == t_rec) %>%
        dplyr::pull(sv_resp)

      base_df <- respts_df %>%
        dplyr::mutate(extent = log(sv_resp / sv_bl)) %>%
        dplyr::rename("t" = t_resp)
    } else {
      stop("bl_mode must be 'ts' or 'point'.")
    }
  }

  if (slope_mode == "lm") {
    lm_df <- base_df %>%
      dplyr::filter(t >= min(tf_slope), t <= max(tf_slope))

    rate_lm <- stats::lm(extent ~ t, data = lm_df)

    return(rate_lm$coefficients[["t"]])
  } else {
    if (slope_mode == "points") {
      rate_df <- base_df %>%
        dplyr::filter(t == min(tf_slope) | t == max(tf_slope)) %>%
        dplyr::arrange(t) %>%
        dplyr::mutate(lim = dplyr::case_when(
          t == min(tf_slope, na.rm = na_rm) ~ "min_t",
          t == max(tf_slope, na.rm = na_rm) ~ "max_t"
        )) %>%
        dplyr::select(lim, extent) %>%
        tidyr::pivot_wider(names_from = lim, values_from = extent) %>%
        dplyr::mutate(rate = (max_t - min_t) / (max(tf_slope) - min(tf_slope)))

      return(rate_df$rate)
    } else {
      stop("slope_mode must be 'lm' or 'points'.")
    }
  }
}
