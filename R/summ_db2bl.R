#' Summarize the values of the state variable in a disturbed system into a baseline
#'
#' Internal function, used in \code{resistance()} and \code{recovery_extent()}
#' to create a baseline value out of the pre-disturbance values in the
#' disturbed system.
#'
#' @inheritParams univar_params
#'
#' @noRd
summ_db2bl <- function(dbts_df, bl_tf, summ_mode, na_rm) {
  summ_f <- match.fun(summ_mode)
  bl_df <- dbts_df[(dbts_df$tdb_i >= min(bl_tf) & dbts_df$tdb_i <= max(bl_tf)),]
  bl <- summ_f(bl_df$svdb_i, na.rm = na_rm)

  return(bl)
}
