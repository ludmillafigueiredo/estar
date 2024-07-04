#' Calculate the extent of recovery
#'
#' \code{recovery_extent} calculates how close a state variable is to its
#' baseline value at a time point specified by the user (usually after recovery
#' has taken place). This distance can be calculated as the log-response ratio
#' between the values in the disturbed system and the baseline
#' or as the difference between the state variables in a disturbed
#' time-series and the baseline. The baseline can be
#' \itemize{
#' \item a value at time \code{t_rec} of the baseline time-series (\code{b_data}) (\code{b = "input"})
#' \item values of the state variable in the disturbed system over a period
#' defined by \code{b_tf}
<<<<<<< HEAD
#' },
#'
#' in both cases, a single baseline value is summarized as the the mean or
#' median (\code{summ_mode}) of the values given.
=======
#' }.
#'
#' In case a certain pre-disturbed period is used as a
#' baseline (\code{b = "d"}), a single value over the specified period
#' \code{b_tf} is summarized as the mean or median (\code{summ_mode}) of the
#' values over that period.
>>>>>>> base_r
#'
#' @param response a string stating whether the stability metric should be
#' calculated using the log-response ratio between the values in the disturbed
#' system and the baseline (\code{response = "lrr"}) or using the state
#' variable values in the disturbed system alone.
#' @param summ_mode A string, stating whether the baseline should be summarized as
#' the mean (\code{summ_mode = "mean"}) or the median (\code{summ_mode = "median"}).
#' Defaults to "mean".
#' @param t_rec An integer, time point at which the extent of recovery should be
#' calculated.
#' @inheritParams univar_params
#'
#' @details Even though it is possible to use a single data value as baseline
#' (by passing a double to \code{b_tf}), it is not recommended, because a
#' single value does not account for any variability in the system arising from,
#' for example, demographic or environmental stochasticity.
#'
#' @return a double, the extent of recovery
#'
#' @examples
#' recovery_extent(
#'   vd_i = "statvar_db", td_i = "time", d_data = aquacomm_resps, response = "lrr",
#'   b = "input", t_rec = 42, vb_i = "statvar_bl", tb_i = "time",
#'   b_data = aquacomm_resps
#' )
#' recovery_extent(
#'   vd_i = "statvar_db", td_i = "time", d_data = aquacomm_resps, response = "diff",
#'   b = "input", t_rec = 42, vb_i = "statvar_bl", tb_i = "time",
#'   b_data = aquacomm_resps
#' )
#' recovery_extent(
#'   vd_i = "statvar_db", td_i = "time", d_data = aquacomm_resps, response = "lrr",
#'   b = "d", t_rec = 42, b_tf = 9
#' )
#' recovery_extent(
#'   vd_i = "statvar_db", td_i = "time", d_data = aquacomm_resps, response = "lrr",
#'   b = "d", t_rec = 42, b_tf = c(5, 10)
#' )
#' recovery_extent(
#'   vd_i = "statvar_db", td_i = "time", d_data = aquacomm_resps, response = "lrr",
#'   b = "d", t_rec = 42, b_tf = c(5, 10), summ_mode = "median"
#' )
#' @export
<<<<<<< HEAD
recovery_extent <- function(vd_i, td_i, d_data, response, b, t_rec,
                            vb_i = NULL, tb_i = NULL, b_data = NULL,
                            b_tf = NULL, summ_mode = "mean",
                            na_rm = TRUE) {

=======
recovery_extent <- function(response,
                            t_rec,
                            summ_mode = "mean",
                            b,
                            b_tf = NULL,
                            vd_i,
                            td_i,
                            d_data,
                            vb_i = NULL,
                            tb_i = NULL,
                            b_data = NULL,
                            na_rm = TRUE) {
>>>>>>> base_r
  dts_df <- format_input("d", vd_i, td_i, d_data)

  if (b == "input") {
    bts_df <- format_input("b", vb_i, tb_i, b_data)

<<<<<<< HEAD
    extent_df <- merge(data.frame("vd_i" = dts_df$vd_i, "t" = dts_df$td_i),
                       data.frame("vb_i" = bts_df$vb_i, "t" = bts_df$tb_i))

    ifelse(!(t_rec %in% extent_df$t),
           stop("Choose a t_rec for which you have input data."),
           extent_df <- extent_df[extent_df$t == t_rec,])
=======
    extent_df <- merge(
      data.frame("vd_i" = dts_df$vd_i, "t" = dts_df$td_i),
      data.frame("vb_i" = bts_df$vb_i, "t" = bts_df$tb_i)
    )

    ifelse(!(t_rec %in% extent_df$t), stop("Choose a t_rec for which you have input data."), extent_df <- extent_df[extent_df$t == t_rec, ])
>>>>>>> base_r
  } else {
    if (b == "d") {
      if (min(b_tf) == max(b_tf)) {
        warning("You are using a single point as baseline. Consider an interval, Details.")
      }
      b <- summ_d2b(dts_df, b_tf, summ_mode, na_rm)
      extent_df <- dts_df[dts_df$td_i == t_rec, ]
      extent_df$vb_i <- b
    } else {
      stop("b must be \"input\" or \"d\".")
    }
  }

  if (response == "lrr") {
    extent_df$extent <- log(extent_df$vd_i / extent_df$vb_i)
  } else if (response == "diff") {
    extent_df$extent <- extent_df$vd_i - extent_df$vb_i
  } else {
    stop("response must be \"lrr\" or \"diff\"")
  }

  return(extent_df$extent)
}
