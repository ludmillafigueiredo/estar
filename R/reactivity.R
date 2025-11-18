#' Calculate the reactivity of a community after disturbance
#'
#' \code{reactitvity} calculates reactivity \eqn{R_a} as the initial rate
#' of return to equilibrium (Downing et al. 2020). Following
#' Neubert et al. (1996), it is calculated as the largest
#' eigenvalue of the Hermitian part of the community matrix
#' \eqn{B}
#' .
#'
#' @param B a matrix, containing the interactions between the species
#' or functional groups in the community.
#' Can be calculated with \code{\link{extractB}} from the
#' fitted MARSS object.
#'
#' @return A single numeric value, the reactivity value.
#' If \eqn{R_a > 0}
#' , the perturbation grows, i.e., the system is initially destabilised.
#' If \eqn{R_a < 0}
#' 0 the perturbation doesn’t grow and the system isn’t initially destabilised.
#'
#' @details
#' \eqn{R_a = \lambda_{\mathrm{dom}}\!\left( H(B) \right)}
#' where
#' \eqn{\lambda_{\mathrm{dom}}}
#' is the dominant eigenvalue, and
#' \eqn{H(B)}
#' is the Rayleigh quotient of
#' \eqn{B}
#' :
#' \eqn{H(B) = \frac{x^{\mathsf{T}} B\, x}{x^{\mathsf{T}} x}}
#' .
#'
#' @seealso
#' [estar::extractB()]
#'
#' @references
#' Neubert, M. G., & Caswell, H. (1997). Alternatives to Resilience for Measuring the Responses of Ecological Systems to Perturbations. Ecology, 78(3), 653–665.
#'
#' Downing, A. L., Jackson, C., Plunkett, C., Lockhart, J. A., Schlater, S. M., & Leibold, M. A. (2020). Temporal stability vs. Community matrix measures of stability and the role of weak interactions. Ecology Letters, 23(10), 1468–1478. \doi{10.1111/ele.13538}
#'
#' @example man/examples/eg_reactivity.R
#'
#' @export
reactivity <- function(B) {
  r_a <- max(eigen((B + t(B))/2, symmetric = TRUE)$values)
  return(r_a)
}
