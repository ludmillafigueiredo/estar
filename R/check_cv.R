#' Verify whether value of state variable falls inside limit
#'
#' @param sv_c a double, the state variable to be checked.
#' @param up_lim a double, the upper limit defined
#' @param low_lim a double, the upper limit defined
#' @noRd
check_lim <- function(sv_c, up_lim, low_lim) {
    (sv_c <= up_lim & sv_c >= low_lim)
}
