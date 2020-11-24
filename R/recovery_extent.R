#' Calculate the extent of recovery.
#'
#' @description Returns the log-ratio response of a state variable in relation to a baseline or between two time steps.
#'
#' @param sv_resp a vector containing the response state variable or a string
#' specifying the name of the column containing said variable in the dataframe
#' provided in \code{data}.
#' @param t_resp a vector containing the time or a string specifying the name
#' of the column containing the time in the dataframe provided in \code{data}.
#' @param data_resp an optional data frame containing the columns storing the
#' response state variable and time.
#' @param bl_mode a string determining whether recovery is calculated in relation
#' to a baseline time series (\code{bl_mode = "ts"}), to a point in the state
#' variable time series (\code{bl_mode = "point"}), or (\code{bl_mode = "period"})
#' to a summary (mean or median, defined by \code{summ_mode}) of the values of
#' the state variable during a period defined by \code{tfsd_bl}.
#' @param t_rec an integer, time step at which extent of recovery should be
#' calculated.
#' @param sv_bl a vector containing the baseline, or a string containing
#' the name of the column in \code{data_bl} containing the baseline.
#' Obligatory argument if (\code{bl_mode = "bl"}).
#' @param t_bl an optional vector containing the time steps for which the baseline
#' was measured, or a string containing the name of the column in \code{data_bl}.
#' Obligatory argument if (\code{bl_mode = "bl"}).
#' @param data_bl an optional data frame containing the columns storing the
#' baseline of the state variable.
#' @param tsv_bl an integer stating the time point of the state variable response
#' time-series that should be used as baseline.
#' Obligatory argument if \code{bl_mode = "point"}.
#' @param tfsv_bl a numerical vector defining the period over which the values of
#' the state variable response should be used to calculate the baseline.
#' Obligatory argument if \code{bl_mode = "point"}.
#' @param summ_mode a string, stating whether the baseline should be summarized as
#' the mean (\code{summ_mode = "mean"}) or the median (\code{summ_mode = "mean"}).
#' @param bl_narm a logical indicating whether NA values should be removed before
#' summarizing the values to be used as baseline.
#' Obligatory argument if \code{bl_mode = "period"}.
#'
#' @return a double, the extent of recovery
#'
#' @examples
#' recovery_extent(
#'   sv_resp = "stat_var", t_resp = "time", data_resp = toy_svts, bl_mode = "ts",
#'   t_rec = 50, sv_bl = "stat_var", t_bl = "time", data_bl = toy_blts
#' )
#' recovery_extent(
#'   sv_resp = "stat_var", t_resp = "time", data_resp = toy_svts, bl_mode = "point",
#'   t_rec = 50, tsv_bl = 9
#' )
#' recovery_extent(
#'   sv_resp = "stat_var", t_resp = "time", data_resp = toy_svts, bl_mode = "period",
#'   t_rec = 50, tfsv_bl = c(5, 10), summ_mode = "mean"
#' )
#' recovery_extent(
#'   sv_resp = "stat_var", t_resp = "time", data_resp = toy_svts, bl_mode = "period",
#'   t_rec = 50, tfsv_bl = c(5, 10), summ_mode = "median"
#' )
#' @export
recovery_extent <- function(sv_resp, t_resp, data_resp, bl_mode, t_rec, sv_bl = NULL,
                            t_bl = NULL, data_bl = NULL, tsv_bl = NULL, tfsv_bl = NULL,
                            summ_mode = NULL, bl_narm = TRUE) {
  respts_df <- format_input(input = "dtb", sv_resp, t_resp, data_resp)
  if (bl_mode == "ts") {
    blts_df <- format_input(input = "udtb", sv_resp, t_resp, data_resp)
    extent_df <- dplyr::left_join(
      dplyr::rename(respts_df, "t" = "t_resp"),
      dplyr::rename(blts_df, "t" = "t_bl"),
      by = c("t")
    ) %>%
      dplyr::filter(t == t_rec)
  } else {
    if (bl_mode == "point") {
      if (is.null(tsv_bl)) {
        stop("Missing tsv_bl argument.")
      }
      svbl_df <- dplyr::filter(respts_df, t_resp == tsv_bl) %>%
        dplyr::rename("sv_bl" = sv_resp)
      extent_df <- respts_df %>%
        dplyr::filter(t_resp == t_rec) %>%
        dplyr::mutate(sv_bl = svbl_df$sv_bl)
      warning("You are using a single point as baseline. Consider an interval.")
    } else {
      if (bl_mode == "period") {
        summ_f <- match.fun(summ_mode)
        svbl_df <- dplyr::filter(
          respts_df,
          t_resp >= tfsv_bl, t_resp <= tfsv_bl
        ) %>%
          dplyr::summarize("sv_bl" = summ_f(sv_resp, na.rm = bl_narm))

        extent_df <- respts_df %>%
          dplyr::filter(t_resp == t_rec) %>%
          dplyr::mutate("sv_bl" = svbl_df$sv_bl)
      } else {
        stop("bl_mode must be 'ts', 'point', of tframe.")
      }
    }
  }

  extent <- extent_df %>%
    dplyr::mutate(extent = log(sv_resp / sv_bl)) %>%
    dplyr::pull(extent)
  return(extent)
}
