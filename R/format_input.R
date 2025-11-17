#' Compose a standardized dataframe to be wrangled by the function
#'
#' @param input a string stating whether the data frame to be created is
#' for the disturbed system (\code{input = "d"}) or for the baseline
#' (\code{input = "b"}))
#' @param v_v a numerical vector passed to the function as \code{v_resp} or
#' \code{v_b}
#' @param t_v a numerical vector passed to the function as \code{t_resp},
#' \code{t_b} or \code{NULL}
#' @param data a dataframe passed to the function as \code{data_resp},
#' \code{data_b} or \code{NULL}
#' 
#' @keywords internal
format_input <- function(input = NULL, v_v = NULL, t_v = NULL, data) {
  if (input == "d") {
    if (is.null(data)) {
      input_df <- data.frame("vd_i" = v_v, "td_i" = t_v)
    } else {
      input_df <- data.frame(vd_i = data[[v_v]], td_i = data[[t_v]])
    }
  } else if (input == "b") {
    if (is.null(data)) {
      input_df <- data.frame("vb_i" = v_v, "tb_i" = t_v)
    } else {
      input_df <- data.frame(vb_i = data[[v_v]], tb_i = data[[t_v]])
    }
  } else {
    stop("'input' argument must be \"d\" or \"b\".")
  }
  return(input_df)
}
