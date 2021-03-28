#' Calculate the persistance of a state variable inside a defined interval
#'
#' \code{persistence} returns the proportion of time the state
#' variable remained inside the interval defined by the baseline's
#' \eqn{\pm} sd. The proportion is calculated in relation to the time
#' period for which persistence should be calculated. ## V: actually tend to exclude persistence altogether from the package.
#' ## V:Let's talk about it at the meeting
#'
#' @inheritParams univar_params
#'
#' @return a double, contained in \[0,1\]
#'
#' @details If the baseline is defined by the pre-distrubed values of the
#' state variable in the disturbed system (\code{bl = "db"}), this pre-disturbed
#' time period used as baseline (\code{bl_tf}) cannot overlap with the time period
#' for which the persistence is to be calculated (\code{metric_tf}), because of redundancy:
#' the values over \code{bl_tf} define the interval for which the values in \code{metric_tf} are
#' checked. If they are the same (or partly, if overlap is partial), the
#' returned value of persistence will be falsely higher.
#'
#' @examples
#' persistence(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_dbts, bl = "db",
#'   bl_tf = c(1, 9), metric_tf = c(50, 100)
#' )
#' persistence(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_dbts, bl = "db",
#'   bl_tf = c(1, 9), metric_tf = c(30, 100)
#' )
#' persistence(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_dbts, bl = "input",
#'   metric_tf = c(50, 100), svbl_i = "stat_var", tbl_i = "time",
#'   bl_data = toy_blts
#' )
#' persistence(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_dbts, bl = "input",
#'   metric_tf = c(30, 100), svbl_i = "stat_var", tbl_i = "time",
#'   bl_data = toy_blts
#' )
#' @export
persistence <- function(svdb_i, tdb_i, db_data = NULL, metric_tf, bl, bl_tf = NULL,
                        svbl_i = NULL, tbl_i = NULL, bl_data = NULL,
                        na_rm = TRUE
                        ) {
    dbts_df <- format_input(input = "db", svdb_i, tdb_i, db_data)

    if (bl == "input") {
        blts_df <- format_input(input = "bl", svbl_i, tbl_i, bl_data) %>%
            dplyr::rename("sv" = svbl_c)
    } else {
        if (bl == "db") {
            if(max(bl_tf) > min(metric_tf)) {
                stop("Baseline overlaps with persistence period. Check Details.")
            }
            blts_df <- dbts_df %>%
            dplyr::ungroup() %>%
            dplyr::filter(tdb_c >= min(bl_tf), tdb_c <= max(bl_tf)) %>%
            dplyr::rename("sv" = svdb_c)

        } else {
            stop("bl must be \"input\" or \"db\".")
        }
    }

    perst_zone <- blts_df %>%
        dplyr::select(sv) %>%
        dplyr::summarize(mean_sv = mean(sv, na.rm = na_rm),
                         sd_sv = sd(sv, na.rm = na_rm)) %>%
        dplyr::summarize(low_lim = mean_sv - sd_sv,
                         high_lim = mean_sv + sd_sv)

    persistence_df <- dbts_df %>%
        dplyr::filter(tdb_c >= min(metric_tf), tdb_c <= max(metric_tf)) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(persist = all(svdb_c >= perst_zone$low_lim,
                                    svdb_c <= perst_zone$high_lim)) %>%
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

