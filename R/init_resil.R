#' Calculate the initial resilience of a community from its community matrix.
#'
#' \code{init_resil} calculates initial resilience
#' \eqn{R_0}
#' as the initial rate of return to equilibrium (Downing et al. 2020).
#' The larger its value, the more stable the system, as its “worst case”
#' initial rate of return to equilibrium is faster (Downing et al. 2020).
#'
#' @param B a matrix, containing the interactions between the species
#' or functional groups in the community.
#' Can be calculated with \code{\link{extractB}} from the
#' fitted MARSS object.
#'
#' @return A single numeric value, the initial resilience.
#' The larger its value, the more stable the system.
#'
#' @details
#' \deqn{
#' R_0 = -\log \!\left(
#'         \sqrt{\lambda_{\mathrm{dom}}\!\left( B^{\mathsf{T}} B \right)}
#'       \right)
#' }
#' where
#' \eqn{\lambda_{\mathrm{dom}}}
#' is the dominant eigenvalue.
#'
#' @references
#' Downing, A. L., Jackson, C., Plunkett, C., Lockhart, J. A., Schlater, S. M., & Leibold, M. A. (2020). Temporal stability vs. Community matrix measures of stability and the role of weak interactions. Ecology Letters, 23(10), 1468–1478. \doi{10.1111/ele.13538}
#'
#' @seealso
#' [estar::extractB()]
#'
#' @example man/examples/eg_init_resil.R
#'
#' @export
init_resil <- function(B) {
  l_dom <- eigen(t(B) * B, symmetric = TRUE)$values[1]
  r_0 <- -log(sqrt(l_dom))
  return(r_0)
}
