#' Calculate the persistence of a state variable inside a defined interval
#'
#' \code{persistence} returns the proportion of time the state
#' variable remained inside the interval defined by the baseline's
#' \eqn{\pm} sd. The proportion is calculated in relation to the time
#' period for which persistence should be calculated.
#'
#' @param bl a string stating whether the baseline is defined by a separate
#' baseline that is specified by the user (\code{bl = "input"}) or by a
#' time period of the disturbed system (\code{bl = "db"}), to be defined by \code{bl_tf}.
#' @param metric_tf a numerical vector, specifying the beginning and end of the
#' time period for which the stability metric should be measured.
#' @param bl_tf a numerical vector, specifying the beginning and end of the
#' pre-disturbance time period for the disturbed time-series that defines
#' the baseline. Obligatory if (\code{bl = "db"}), see 'Details'.
#'
#' @inheritParams univar_params
#'
#' @return a double, contained in \[0,1\]
#'
#' @details If the baseline is defined by the pre-disturbed values of the
#' state variable in the disturbed system (\code{bl = "db"}), this pre-disturbed
#' time period used as baseline (\code{bl_tf}) cannot overlap with the time period
#' for which the persistence is to be calculated (\code{metric_tf}), because of
#' redundancy: the values over \code{bl_tf} define the interval for which the
#' values in \code{metric_tf} are checked. If they are the same (or partly, if
#' overlap is partial), the returned value of persistence will be falsely higher.
#'
#' @examples
#' persistence(
#'   svdb_i = "statvar_db", tdb_i = "time", db_data = aquacomm_resps, bl = "db",
#'   bl_tf = c(1, 9), metric_tf = c(50, 100)
#' )
#' persistence(
#'   svdb_i = "statvar_db", tdb_i = "time", db_data = aquacomm_resps, bl = "db",
#'   bl_tf = c(1, 9), metric_tf = c(30, 100)
#' )
#' persistence(
#'   svdb_i = "statvar_db", tdb_i = "time", db_data = aquacomm_resps, bl = "input",
#'   metric_tf = c(50, 100), svbl_i = "statvar_bl", tbl_i = "time",
#'   bl_data = aquacomm_resps
#' )
#' persistence(
#'   svdb_i = "statvar_db", tdb_i = "time", db_data = aquacomm_resps, bl = "input",
#'   metric_tf = c(30, 100), svbl_i = "statvar_bl", tbl_i = "time",
#'   bl_data = aquacomm_resps
#' )
#' @export
persistence <- function(svdb_i, tdb_i, db_data = NULL, metric_tf, bl, bl_tf = NULL,
                        svbl_i = NULL, tbl_i = NULL, bl_data = NULL,
                        na_rm = TRUE
                        ) {
    dbts_df <- format_input(input = "db", svdb_i, tdb_i, db_data)

    if (bl == "input") {
        blts_df <- format_input(input = "bl", svbl_i, tbl_i, bl_data) %>%
            dplyr::rename("sv" = svbl_i)
    } else {
        if (bl == "db") {
            if(max(bl_tf) > min(metric_tf)) {
                stop("Baseline overlaps with persistence period. Check Details.")
            }
            blts_df <- dbts_df %>%
            dplyr::ungroup() %>%
            dplyr::filter(tdb_i >= min(bl_tf), tdb_i <= max(bl_tf)) %>%
            dplyr::rename("sv" = svdb_i)

        } else {
            stop("bl must be \"input\" or \"db\".")
        }
    }

    perst_zone <- blts_df %>%
        dplyr::select(sv) %>%
        dplyr::summarize(mean_sv = mean(sv, na.rm = na_rm),
                         sd_sv = stats::sd(sv, na.rm = na_rm)) %>%
        dplyr::summarize(low_lim = mean_sv - sd_sv,
                         high_lim = mean_sv + sd_sv)

    persistence_df <- dbts_df %>%
        dplyr::filter(tdb_i >= min(metric_tf), tdb_i <= max(metric_tf)) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(persist = all(svdb_i >= perst_zone$low_lim,
                                    svdb_i <= perst_zone$high_lim)) %>%
        dplyr::group_by(persist) %>%
        dplyr::summarize(n_p = dplyr::n()) %>%
        dplyr::ungroup()  %>%
        dplyr::mutate(persistence = n_p/sum(n_p)) %>%
        dplyr::filter(persist == TRUE)

    ## necessary if all persist values are FALSE, and data frame ends up empty
    if (dim(persistence_df)[1] == 0) {
        persistence = 0
        return(persistence)
    } else {
        return(persistence_df$persistence)
    }
}

