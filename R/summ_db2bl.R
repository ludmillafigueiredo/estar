#' Summarize the values of the state variable in a disturbed system into a baseline
#'
#' Internal function, used in \code{resistance()} and \code{recovery_extent()}
#' to create a baseline value out of values the time-series characterizing the
#' disturbed system.
#'
#' @inheritParams common_parameters
#' @noRd
summ_db2bl <- function(dbts_df, bl_tf, summ_mode, na_rm) {
  summ_f <- match.fun(summ_mode)
  bl <- dbts_df %>%
    dplyr::filter(tdb_c >= min(bl_tf), tdb_c <= max(bl_tf)) %>%
    dplyr::ungroup() %>%
    dplyr::summarize("svbl_c" = summ_f(svdb_c, na.rm = na_rm)) %>%
    dplyr::pull(svbl_c)
  return(bl)
}
