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
#' @return A numeric, the reactivity value.
#'
#' @seealso
#' [estar::extractB()]
#'
#' @references
#' Neubert, M. G., & Caswell, H. (1997). Alternatives to Resilience for Measuring the Responses of Ecological Systems to Perturbations. Ecology, 78(3), 653–665.
#'
#' @example man/examples/eg_reactivity.R
#'
#' @export
reactivity <- function(B) {
  r_a <- max(eigen((B + t(B))/2, symmetric = TRUE)$values)
  return(r_a)
}
