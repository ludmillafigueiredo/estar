#' Calculate the reactivity of a community from its community matrix.
#'
#' \code{reactitvity} calculates reactivity as the initial rate
#' of return to equilibrium (Downing et al. 2020). Following
#' Neubert et al. (1996), it is calculated as the largest
#' eigenvalue of the Hermitian part of the community matrix $B$.
#'
#' @param B a matrix, containing the interactions between the species
#' or functional groups in the community.
#' Can be calculated with \code{\link{extractB}} from the
#' fitted MARSS object.
#'
#' @return a numeric, the reactivity value
#' VIK: please add an example
#'
#' @export
reactivity <- function(B){
  react <- max(eigen((B+t(B))/2)$values)
}
