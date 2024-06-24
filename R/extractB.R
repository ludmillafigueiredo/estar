#' Extract the community matrix (B) estimated by a MARSS model
#'
#' @param marss_res MARSS object returned by \code{\link[MARSS]{MARSS}}
#' @param states_names a string vector containing the names of species/groups for which interactions were estimated
#'
#' @return a named matrix
#'
#' VIK: please add an example
#' @export
extractB <- function(marss_res, states_names = NULL){
  if (is.null(states_names)) {
    states_names <- 1:sqrt(length(stats::coef(marss_res)$B))
  }

  stats::coef(marss_res)$B |>
    matrix(nrow = length(states_names), ncol = length(states_names), byrow = FALSE,
           dimnames = list(states_names, states_names))
}
