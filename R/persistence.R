#' Calculate the persistance of a state variable inside a defined interval
#'
#' @description Return the proportion of time the state variable remained
#' inside the interval defined by the baseline's mean \eqn{\pm} sd. The proportion
#' is calculated in relation to the time frame for which persistence should be
#' calculated.
#'
#' @param svdb_i a numeric vector containing the state variable in the
#' disturbed system or a string specifying the name of the column
#' containing said variable in the dataframe provided in \code{db_data}.
#' @param tdb_i a numeric vector containing the time or a string specifying the
#' name of the column containing the time in the dataframe provided
#' in \code{db_data}.
#' @param db_data an optional data frame containing the time-series of the values
#' of the state variable in a state considered to be disturbed.
#' @param perst_tf a numerical vector, specifying the beginning and end of the
#' time interval during which persistence should be measured.
#' @param bl a string stating whether the baseline is defined by a separate
#' baseline that is input (\code{bl = "input"}) or by a certain time period of
#' the disturbed system (\code{bl = "db"}).
#' @param bl_tf a numerical vector, specifying the beginning and end of the
#' time interval that defines the baseline (either in the disturbed time-series,
#' or in the baseline input). See Details.
#' @param svbl_i a numeric vector containing the state variable in the baseline,
#' or a string for the name of the column in \code{bl_data} containing said
#' variable in the baseline.
#' Obligatory argument if \code{bl = "bl"}.
#' @param tbl_i an optional numeric vector containing the time steps for which
#' the baseline was measured, or a string containing the name of the column in
#' \code{bl_data}.
#' Obligatory argument if \code{bl = "bl"}.
#' @param bl_data an optional data frame containing the columns storing the baseline
#' of the state variable.
#' @param na_rm a logical determining whether NAs should be taken out for the
#' estimation of variation
#'
#' @return a double, contained in \[0,1\]
#' 
#' @details If the baseline is defined by values of the state variable along the
#' disturbed time-series (\code{bl = "db"}), the time frame used as baseline
#' (\code{bl_tf}) cannot overlap with the time frame for which the persistence
#' is to be calculated (\code{perst_tf}), because of redundancy: the values over
#' \code{bl_tf} define the interval for which the values in \code{perst_tf} are
#' checked. If they are the same (or partly, if overlap is partial), the
#' returned value of persistence will be falsely higher.
#' 
#' @examples
#' persistence(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_dbts, bl = "db",
#'   bl_tf = c(1, 9), perst_tf = c(50, 100), svbl_i = NULL, tbl_i = NULL, bl_data = NULL,
#'   na_rm = TRUE
#' )
#' persistence(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_dbts, bl = "bl",
#'   bl_tf = c(50, 100), perst_tf = c(50, 100), svbl_i = NULL, tbl_i = NULL, bl_data = NULL,
#'   na_rm = TRUE
#' )
#' @export
persistence <- function(svdb_i, tdb_i, db_data = NULL, perst_tf, bl, bl_tf,
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
            if(max(bl_tf) > min(perst_tf)) {
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
        dplyr::summarize(mean_sv = mean(sv, na.rm = na_rm), sd_sv = sd(sv, na.rm = na_rm)) %>%
        dplyr::summarize(low_lim = mean_sv - sd_sv,
                         high_lim = mean_sv + sd_sv)

    persistence <- dbts_df %>%
        dplyr::filter(tdb_c >= min(perst_tf), tdb_c <= max(perst_tf)) %>%
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
