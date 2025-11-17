#' Calculate the asymptotic resilience of a community from its
#' community matrix.
#'
#' \code{asympt_resil} calculates asymptotic resilience
#'
#' \eqn{R_a}
#'
#' as the slowest long-term asymptotic rate of return to
#' equilibrium after a pulse perturbation (Arnoldi et al. 2016,
#' Downing et al. 2020).
#'
#' @param B a matrix, containing the interactions between the species
#' or functional groups in the community. Can be calculated with
#' \code{\link{extractB}} from the fitted MARSS object.
#'
#' @return A single positive numeric value, asymptotic rate of return to
#' equilibrium after a pulse perturbation.The larger its value, the more
#' stable the system.
#'
#' @details
#'
#' \deqn{
#' R_a = \lambda_{\mathrm{dom}}\!\left( H(B) \right)
#' }
#' where
#'
#' \eqn{\lambda_{\mathrm{dom}}}
#'
#' is the dominant eigenvalue, and
#'
#' \eqn{H(B)}
#'
#' is the Rayleigh quotient of
#'
#' \eqn{B}
#'
#' :
#'
#' \eqn{H(B) = \frac{x^{\mathsf{T}} B\, x}{x^{\mathsf{T}} x}}
#'
#' .
#'
#' @references
#' Arnoldi et al. (2016). Resilience, reactivity and variability: A mathematical comparison of ecological stability measures. Journal of Theoretical Biology, 389, 47–59. \doi{10.1016/j.jtbi.2015.10.012}
#'
#' Downing, A. L., Jackson, C., Plunkett, C., Lockhart, J. A., Schlater, S. M., & Leibold, M. A. (2020). Temporal stability vs. Community matrix measures of stability and the role of weak interactions. Ecology Letters, 23(10), 1468–1478. \doi{10.1111/ele.13538}
#'
#' @seealso
#' [estar::extractB()]
#'
#' @example man/examples/eg_asympt_resil.R
#'
#' @export
asympt_resil <- function(B) {
  l_dom <- eigen(B, symmetric = TRUE)$values[1]
  r_inf <- -log(abs(l_dom))
  return(r_inf)
}
