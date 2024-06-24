#' Calculate the asymptotic resilience of a community from its
#' community matrix.
#'
#' \code{init_resil} calculates asymptotic resilience as
#' the slowest long-term asymptotic rate of return to
#' equilibrium after a pulse perturbation (Arnoldi et al. 2016,
#' Downing et al. 2020).
#'
#' @param B a matrix, containing the interactions between the species
#' or functional groups in the community.
#' Can be calculated with \code{\link{extractB}} from the
#' fitted MARSS object.
#'
#' @return a numeric, the asymptotic resilience
#'
#' @export
asympt_resil <- function(B){
    l_dom <- eigen(B)$values[1]
    i_s <- -log(abs(l_dom))
}
