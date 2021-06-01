#' Summarize the values of the state variable in a disturbed system into a baseline
#'
#' Internal function, used in \code{resistance()} and \code{recovery_extent()}
#' to create a baseline value out of the pre-disturbance values in the
#' disturbed system.
#'
#' @inheritParams univar_params
#' @noRd
summ_db2bl <- function(dbts_df, bl_tf, summ_mode, na_rm) {
  summ_f <- match.fun(summ_mode)
  bl <- dbts_df %>%
    dplyr::filter(tdb_i >= min(bl_tf), tdb_i <= max(bl_tf)) %>%
    dplyr::ungroup() %>%
    dplyr::summarize("svbl_i" = summ_f(svdb_i, na.rm = na_rm)) %>%
    dplyr::pull(svbl_i)
  return(bl)
}
