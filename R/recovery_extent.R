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
#' to a baseline time series (\code{bl_mode = "ts"}) or to a point in the state
#' variable time series (\code{bl_mode = "point"}).
#' @param t_rec an integer, time step at which extent of recovery should be
#' calculated.
#' @param sv_bl a vector containing the baseline, or a string containing
#' the name of the column in \code{data_bl} containing the baseline.
#' Obligatory argument if (\code{slope_mode = "bl"}).
#' @param t_bl an optional vector containing the time steps for which the baseline
#' was measured, or a string containing the name of the column in \code{data_bl}.
#' Obligatory argument if (\code{slope_mode = "bl"}).  ## V: par-r slope_mode seems to be not documented
#' @param data_bl an optional data frame containing the columns storing the
#' baseline of the state variable.
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
#'   t_rec = 9, sv_bl = "stat_var", t_bl = "time", data_bl = toy_blts
#' )
#' @export
recovery_extent <- function(sv_resp, t_resp, data_resp, bl_mode, t_rec, sv_bl = NULL,
                            t_bl = NULL, data_bl = NULL) {
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
    extent_df <- dplyr::left_join(  ## V: does not work
        dplyr::rename(respts_df),
        dplyr::rename(blts_df),
        by = c("t_resp" = "t_bl")
    ) %>%
        dplyr::rename("t" = t_resp) %>%
      dplyr::filter(t == t_rec)
  } else {
    if (bl_mode == "point") {
      svbl_df <- dplyr::filter(respts_df, t_resp == t_rec) %>%  ## V: I think this will not work because you are using a time step for caluclating the recovery,
          ## but you need a time step for the baseline time point - analogous to how you did it with resistancs
        dplyr::rename("sv_bl" = sv_resp)
      extent_df <- respts_df %>%
        dplyr::filter(t_resp == t_rec) %>%
        dplyr::mutate(sv_bl = svbl_df$sv_bl)  ## V: so here I expect you will have sv_bl exactly equal oto sv_resp....
      warning("You are using a single point as baseline.")
    } else {
      stop("bl_mode must be 'ts' or 'point'.")
    }
  }

  extent <- extent_df %>%
    dplyr::mutate(extent = log(sv_resp / sv_bl)) %>%
    dplyr::pull(extent)
  return(extent)
}
