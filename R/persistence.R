#' Calculate the persistance of a state variable inside a zone
#'
#' @description Return the number of time steps the state variable remained
#' inside 
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
#' @param svbl_i a numeric vector containing the state variable in the baseline,
#' or a string for the name of the column in \code{bl_data} containing said
#' variable in the baseline.
#' Obligatory argument if \code{response = "lrr"}.
#' @param tbl_i an optional numeric vector containing the time steps for which
#' the baseline was measured, or a string containing the name of the column in
#' \code{bl_data}.
#' Obligatory argument if \code{response = "lrr"}.
#' @param bl_data an optional data frame containing the columns storing the baseline
#' of the state variable.
#' @param na_rm a logical determining whether NAs should be taken out for the
#' estimation of variation
#' 
#' @return a double, the proportion of \code{perst_tf} during which the system persisted
#'
#' @details Persistence is defined here as the proportion of time, in relation to a
#' user-defined time frame, during which the response value (eith state variable or
#' the log-ratio to baseline) stays inside the interval define by the mean and standard
#' deviation of the response value during the user-defined time frame.
#' @examples
#' persistence(
#'   svdb_i = "stat_var", tdb_i = "time", db_data = toy_dbts, response = "sv",
#'   perst_tf = c(50, 100), svbl_i = NULL, tbl_i = NULL, bl_data = NULL,
#'   bl_tf = NULL, na_rm = TRUE
#' )
#' @export
persistence <- function(svdb_i, tdb_i, db_data = NULL, response, perst_tf,
                        svbl_i = NULL, tbl_i = NULL, bl_data = NULL,
                        bl_tf = NULL, na_rm = TRUE
                        ) {
    dbts_df <- format_input(input = "db", svdb_i, tdb_i, db_data)
    
    response_df <- sort_response(response, dbts_df, svbl_i, tbl_i, bl_data) %>%
        dplyr::filter(t >= min(perst_tf), t <= max(perst_tf))

    persistence <- response_df %>%
        dplyr::mutate(low_lim = mean(response_df$response, na.rm = na_rm) -
                          sd(response_df$response, na.rm = na_rm),
                      high_lim = mean(response_df$response, na.rm = na_rm) +
                          sd(response_df$response, na.rm = na_rm)) %>%
        rowwise() %>%
        dplyr::mutate(persist = all(response >= low_lim, response <= high_lim)) %>%
        dplyr::group_by(persist) %>%
        dplyr::summarize(n_p = n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(persistence = n_p/sum(n_p)) %>%
        dplyr::filter(persist == TRUE) %>%
        dplyr::pull(persistence)
    return(persistence)
}
