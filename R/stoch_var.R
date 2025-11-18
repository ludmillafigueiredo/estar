#' Calculate the intrinsic stochastic invariability of a
#' community from its community matrix.
#'
#' \code{stoch_var} calculates the intrinsic stochastic
#' ( \eqn{I_S} )
#' invariability - a theoretical equivalent of the
#' univariate measure of invariability (Arnoldi et al. 2016).
#'
#' @param B a matrix, containing the interactions between the species
#' or functional groups in the community. Can be calculated
#' with \code{\link{extractB}} from the fitted MARSS object.
#'
#' @return A numeric, the stochastic variability. The larger its value,
#' the more stable the system, as its rate of return to equilibrium is higher.
#'
#' @details
#' \deqn{
#' I_S = \frac{1}{\lVert B^{-1} \rVert}
#' }
#'
#' where \eqn{\lVert B^{-1} \rVert} is the spectral norm of the inverse
#' of the matrix \eqn{B}. The spectral norm is computed as the dominant
#' eigenvalue of the matrix
#'
#' \deqn{
#' B \otimes I + I \otimes B
#' }
#'
#' where \eqn{I} is the identity matrix.
#'
#' @references
#' Arnoldi, J.-F., Loreau, M., & Haegeman, B. (2016). Resilience, reactivity and variability: A mathematical comparison of ecological stability measures. Journal of Theoretical Biology, 389, 47–59. \doi{10.1016/j.jtbi.2015.10.012}
#'
#' @example man/examples/eg_stoch_var.R
#'
#' @export
stoch_var <- function(B) {
  id <- diag(nrow(B))
  kron_sum <- Matrix::kronecker(B, id) + Matrix::kronecker(id, B)
  nu_s <- norm(solve(kron_sum), type = '2')
  i_s <- 1 / 2 * nu_s
  return(i_s)
}
