#' Compose a standardized dataframe to be wrangled by the function
#'
#' @param input a string stating whether the data frame to be created is
#' for the disturbed system (\code{input = "dtb"}) or for the baseline
#' (\code{input = "bl"}))
#' @param sv_v a numerical vector passed to the function as \code{sv_resp} or
#' \code{sv_bl}
#' @param t_v a numerical vector passed to the function as \code{t_resp},
#' \code{t_bl} or \code{NULL}
#' @param data a dataframe passed to the function as \code{data_resp},
#' \code{data_bl} or \code{NULL}
format_input <- function(input, sv_v, t_v, data) {
  if (input == "db") {
    if (is.null(data)) {
      input_df <- data.frame("svdb_c" = sv_v, "tdb_c" = t_v)
    } else {
      input_df <- dplyr::select(data,
        "svdb_c" = dplyr::all_of(sv_v),
        "tdb_c" = dplyr::all_of(t_v)
      )
    }
  } else {
      if(input == "bl"){
          if (is.null(data)) {
              input_df <- data.frame("svbl_c" = sv_v, "tbl_c" = t_v)
          } else {
              input_df <- dplyr::select(data,
                                        "svbl_c" = dplyr::all_of(sv_v),
                                        "tbl_c" = dplyr::all_of(t_v)
                                        )
          }
      } else {
          stop("'input' argument must be \"db\" or \"bl\".")
      }
  }
}
