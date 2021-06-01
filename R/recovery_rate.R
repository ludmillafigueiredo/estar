#' Calculate the rate of recovery.
#'
#' \code{recovery_rate} returns the rate of recovery calculated as the slope of
#' a linear model which uses the time as a predictor of the response.
#' The response can be the state variable in a disturbed system, or the
#' log-response ratio (LRR) of the state variable in the disturbed system
#' compared to the baseline.
#'
#' @inheritParams univar_params
#'
#' @return a double, the rate of recovery
#'
#' @examples
#' recovery_rate(
#'   svdb_i = "statvar_db", tdb_i = "time", db_data = aquacomm_resps, bl = "db",
#'   metric_tf = c(12, 50)
#' )
#' recovery_rate(
#'   svdb_i = "statvar_db", tdb_i = "time", db_data = aquacomm_resps, bl = "input",
#'   metric_tf = c(12, 50), svbl_i = "statvar_bl", tbl_i = "time", bl_data = aquacomm_resps
#' )
#' @export
recovery_rate <- function(svdb_i, tdb_i, db_data, bl, metric_tf,
                          svbl_i = NULL, tbl_i = NULL, bl_data = NULL, na_rm = TRUE) {
  dbts_df <- format_input(input = "db", svdb_i, tdb_i, db_data)
  if (bl == "input") {
    blts_df <- format_input(input = "bl", svbl_i, tbl_i, bl_data)
    base_df <- dplyr::left_join(
      dplyr::rename(dbts_df, "t" = tdb_i),
      dplyr::rename(blts_df, "t" = tbl_i),
      by = "t"
    ) %>%
      dplyr::mutate(extent = log(svdb_i / svbl_i)) %>%
      dplyr::select(t, extent)
  } else {
    if (bl == "db") {
      base_df <- dbts_df %>%
        dplyr::rename(
          "extent" = svdb_i,
          "t" = tdb_i
        )
    } else {
      stop("bl must be \"input\" or \"db\".")
    }
  }
  lm_df <- base_df %>%
    dplyr::filter(t >= min(metric_tf), t <= max(metric_tf))

  rate_lm <- stats::lm(extent ~ t, data = lm_df)

  return(rate_lm$coefficients[["t"]])
}
