#' Summarize the values of the state variable in a disturbed system into a baseline
#'
#' Internal function, used in \code{resistance()} and \code{recovery_extent()}
#' to create a baseline value out of the pre-disturbance values in the
#' disturbed system.
#'
#' @inheritParams univar_params
#' @keywords internal
summ_d2b <- function(dts_df, b_tf, summ_mode, na_rm) {
  summ_f <- match.fun(summ_mode)
  b_df <- dts_df[(dts_df$td_i >= min(b_tf) &
                    dts_df$td_i <= max(b_tf)), ]
  b <- summ_f(b_df$vd_i, na.rm = na_rm)

  return(b)
}
