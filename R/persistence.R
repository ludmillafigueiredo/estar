#' Calculate the persistance of a state variable inside a defined interval
#'
#' \code{persistence} returns the proportion of time the state
#' variable remained inside the interval defined by the baseline's
#' mean \eqn{\pm} sd. The proportion is calculated in relation to the time
#' frame for which persistence should be calculated.
#' 
#' @param metric_tf a numerical vector, specifying the beginning and end of the
#' time interval during which persistence should be measured.
#' @param bl a string stating whether the baseline is defined by a separate
#' baseline that is input (\code{bl = "input"}) or by a certain time period of
#' the disturbed system (\code{bl = "db"}).
#' @param bl_tf a numerical vector, specifying the beginning and end of the
#' time interval that defines the baseline (either in the disturbed time-series,
#' or in the baseline input). See Details.
#' @inheritParams common_parameters
#' 
#' @return a double, contained in \[0,1\]
#' 
#' @details If the baseline is defined by values of the state variable along the
#' disturbed time-series (\code{bl = "db"}), the time frame used as baseline
#' (\code{bl_tf}) cannot overlap with the time frame for which the persistence
#' is to be calculated (\code{metric_tf}), because of redundancy: the values over
#' \code{bl_tf} define the interval for which the values in \code{metric_tf} are
#' checked. If they are the same (or partly, if overlap is partial), the
#' returned value of persistence will be falsely higher.
#' 
#' @examples
#' persistence(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_dbts, bl = "db",
#'   bl_tf = c(1, 9), metric_tf = c(50, 100), svbl_i = NULL, tbl_i = NULL,
#'   bl_data = NULL, na_rm = TRUE
#' )
#' persistence(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_dbts, bl = "bl",
#'   bl_tf = c(50, 100), metric_tf = c(50, 100), svbl_i = NULL, tbl_i = NULL,
#'   bl_data = NULL, na_rm = TRUE
#' )
#' @export
persistence <- function(svdb_i, tdb_i, db_data = NULL, metric_tf, bl, bl_tf,
                        svbl_i = NULL, tbl_i = NULL, bl_data = NULL,
                        na_rm = TRUE
                        ) {
    dbts_df <- format_input(input = "db", svdb_i, tdb_i, db_data)

    if (bl == "input") {
        blts_df <- format_input(input = "bl", svbl_i, tbl_i, bl_data) ##%>%
            dplyr::rename("t" = tbl_c,
                          "sv" = svbl_c)
    } else {
        if (bl == "db") {
            if(max(bl_tf) > min(metric_tf)) {
                stop("Baseline overlaps with persistence period. Check Details.")
            }
            blts_df <- dbts_df %>%
            dplyr::rename("t" = tdb_c,
                          "sv" = svdb_c)
        } else {
            stop("bl must be \"input\" or \"db\".")
        }
    }

    perst_zone <- blts_df %>%
        dplyr::ungroup() %>%
        dplyr::filter(t >= min(bl_tf), t <= max(bl_tf)) %>%
        dplyr::select(sv) %>%
        dplyr::summarize(mean_sv = mean(sv, na.rm = na_rm),
                         sd_sv = sd(sv, na.rm = na_rm)) %>%
        dplyr::summarize(low_lim = mean_sv - sd_sv,
                         high_lim = mean_sv + sd_sv)

    persistence <- dbts_df %>%
        dplyr::filter(tdb_c >= min(metric_tf), tdb_c <= max(metric_tf)) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(persist = all(svdb_c >= perst_zone$low_lim, svdb_c <= perst_zone$high_lim)) %>%
        dplyr::group_by(persist) %>%
        dplyr::summarize(n_p = dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::filter(persist == TRUE) %>%
        dplyr::mutate(persistence = n_p/sum(n_p)) %>%
        dplyr::pull(persistence)
    return(persistence)
}
