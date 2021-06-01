#' Extract the community matrix (B) estimated by a MARSS model
#'
#' @param marss_res MARSS object returned by \code{\link[MARRS]{MARSS}}
#' @param states_names a string vector containing the names of species/groups for which interactions were estimated
#'
#' @return a named matrix
#'
#' @export
extractB <- function(marss_res, states_names){
  stats::coef(marss_res)$B %>%
    matrix(nrow = length(states_names), ncol = 5, byrow = FALSE,
           dimnames = list(states_names, states_names))
}
