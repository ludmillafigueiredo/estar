#' Calculate the initial resilience of a community from its community matrix.
#'\code{initi_resil} calculates initial resilience as the initial rate of return to equilibrium (Downing et al. 2020).
#' The larger its value, the more stable the system, as its “worst case” initial rate of return to equilibrium is faster (Downing et al. 2020).
#'
#' @param B a matrix, containing the species or functional groups in the community. Can be calculated with \code{\link{extractB}} from the
#' fitted MARSS object.
#'
#' @return a numeric, the initial resilience
#'
#' @export
init_resil <- function(B){
    l_dom <- eigen(t(B)*B)$values[1]
    i_s <- -log(sqrt(l_dom))
}
