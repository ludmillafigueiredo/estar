#' Calculate the invariability of a state variable after disturbance.
#'
#' @description Return the temporal invariability of the state variable.
#' Can be aclulated as the inverse of coefficient of variation or
#' as the standard deviation of residuals of the linear model that uses
#' as the predictor  the time and as the response variable the log-response
#' ratio of the state variable in the disturbed system and in the baseline.
#'
#' @param sv_resp a numeric vector containing the state variable in the disturbed system
#'  or a string specifying the name of the column containing said variable in
#'  the dataframe provided in \code{data}.
#' @param t_resp a numeric vector containing the time or a string specifying the
#' name of the column containing the time in the dataframe provided
#' in \code{data}.
#' @param mode a string stating whether invariability should be calculated
#' from the coefficient of variation of the state variable \code{mode = "cv"},
#' or from the linear model between the response and time \code{"lm_res"}.
#' The details of the two modes as explained in 'Details'.
#' @param tf_resp a numeric vector, specifying the beginning and end of the
#' interval of \code{sv} values, from which invariability should be calculated,
#' or the specific time values defining this interval, if \code{t} is provided.
#' @param na_rm a logical indicating whether NA values should be removed before processing,
#' defaults to TRUE.
#' @param data_resp an optional data frame containing the columns storing the
#' response state variable and time.
#' @param sv_bl a numeric vector containing the state variable in the baseline,
#' or a string containing the name of the column in \code{data_bl} containing said
#' variable in the baseline.
#' Obligatory argument if \code{mode = "lm_res"}.
#' @param t_bl an optional numeric vector containing the time steps for which
#' the baseline was measured, or a string containing the name of the column in \code{data_bl}.
#' Obligatory argument if \code{mode = "lm_res"}.
#' @param tf_bl a numeric vector, specifying the beginning and end of the
#' interval of \code{sv_bl} values from which invariability should be calculated,
#' or the specific time values defining this interval, if \code{data_bl} is provided.
#' Obligatory argument if \code{mode = "lm_res"}.
#' @param data_bl an optional data frame containing the columns storing the baseline
#' of the state variable.
#'
#' @return a numeric, the invariability value.
#'
#' @examples
#' invariability(
#'   sv_resp = "stat_var", t_resp = "time", mode = "cv",
#'   tf_resp = c(11, 50), data_resp = toy_svts
#' )
#' invariability(
#'   sv_resp = toy_svts$stat_var, t_resp = toy_svts$time, mode = "cv",
#'   tf_resp = c(11, 50)
#' )
#' invariability(
#'   sv_resp = "stat_var", t_resp = "time", mode = "lm_res",
#'   tf_resp = c(11, 50), data_resp = toy_svts, sv_bl = "stat_var",
#'   t_bl = "time", tf_bl = c(11, 50), data_bl = toy_blts
#' )
#' invariability(
#'   sv_resp = toy_svts$stat_var, t_resp = toy_svts$time,
#'   tf_resp = c(11, 50), mode = "lm_res", sv_bl = toy_blts$stat_var,
#'   t_bl = toy_blts$time, tf_bl = c(11, 50)
#' )
#' @details
#' Invariance can be calculated as the inverse of the coefficient of variation
#' (\code{mode = "cv"} or as the standard deviation of the residuals of the linear model
#' with the predictor being the time and the response being the log response ratio
#' of the state variable and the baseline.
#' @export
invariability <- function(sv_resp, t_resp, mode, tf_resp, data_resp = NULL,
                          sv_bl = NULL, t_bl = NULL, tf_bl = NULL, data_bl = NULL, na_rm = TRUE) {
  if (is.null(data_resp)) {
    respts_df <- data.frame("sv_resp" = sv_resp, "t_resp" = t_resp)
  } else {
    respts_df <- data_resp %>%
      dplyr::select(
        "sv_resp" = dplyr::all_of(sv_resp),
        "t_resp" = dplyr::all_of(t_resp)
      )
  }

  respts_df <- respts_df %>%
    dplyr::filter(t_resp >= min(tf_resp), t_resp <= max(tf_resp))  ## V: this should be min or max of tf_resp, FIXED

  if (mode == "cv") {
    sv_vct <- dplyr::pull(respts_df, sv_resp)

    if (any(is.na(sv_vct))) {
      message("NAs detected among the entries of the state variable")

      if (sum(!is.na(sv_vct)) < 10) {
        warning("Less than 10 data points are available for measuring invariability.")
      }
    }

    invar <- 1 / (stats::sd(sv_vct, na.rm = na_rm) / mean(sv_vct, na.rm = na_rm))

    return(invar)
  } else {
    if (mode == "lm_res") {
      if (is.null(data_bl)) {
        blts_df <- data.frame("sv_bl" = sv_bl, "t_bl" = t_bl)
      } else {  ## V: there should be an error message here also if sv_bl or t_bl are not specified
        blts_df <- data_bl %>%
          dplyr::select(
            "sv_bl" = dplyr::all_of(sv_bl),
            "t_bl" = dplyr::all_of(t_bl)
          )
      }

      blts_df <- blts_df %>%
        dplyr::filter(t_bl >= min(tf_bl), t_bl <= max(tf_bl))  ## V: these should be min or max of tf_bl, FIXED

      invar_df <- dplyr::inner_join(respts_df,
        blts_df,
        by = c("t_resp" = "t_bl")
      ) %>%
        dplyr::mutate(lrr = log(sv_resp / sv_bl)) %>%
        dplyr::rename("t" = t_resp)

      invar <- 1 / stats::sd(stats::lm(invar_df$lrr ~ invar_df$t)$residuals)

      return(invar)
    } else {
      stop("Specify mode of invariability to calculate:\n\"cv\" or \"lm_res\"")
    }
  }
}
