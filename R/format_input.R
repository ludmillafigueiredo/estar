#' Compose a standardized dataframe to be wrangled by the function
#'
#' @param input a string stating whether the time-series of disturbed values of
#' the state variable (\code{input = "dtb"}).
#' @param sv_vct a numerical vector passed to the function as \code{sv_resp} or
#' \code{sv_bl}
#' @param t_vct a numerical vector passed to the function as \code{t_resp},
#' \code{t_bl} or \code{NULL}
#' @param data a dataframe passed to the function as \code{data_resp},
#' \code{data_bl} or \code{NULL}
format_input <- function(input, sv_vct, t_vct, data) {

    if (is.null(data)) {
        input_df <- data.frame("sv_col" = sv_vct, "sv_col" = t_vct)
    } else {
        if (input == "dtb") {
            input_df <- dplyr::select(data,
                                      "svr_col" = dplyr::all_of(sv_vct),
                                      "tr_col" = dplyr::all_of(t_vct)
                                      )
        } else {
            
            input_df <- dplyr::select(data,
                                      "svbl_col" = dplyr::all_of(sv_vct),
                                      "tbl_col" = dplyr::all_of(t_vct)
                                      )
        }
        
    }
}
