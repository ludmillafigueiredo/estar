#' Calculate the maximal amplification of a community after disturbance
#'
#' \code{max_amp} calculates the maximal amplification ( \eqn{A_{max}} ) as
#' the euclidean norm of a community matrix
#' \eqn{B}
#' (Neubert et al. 1996). It uses the \code{expmat} function of
#' the \code{hesim} package to calculate the exponential of the community matrix
#' \eqn{B}
#' , and then its Euclidean norm.
#'
#' @param B a matrix, containing the interactions between the species
#' or functional groups in the community.
#' Can be calculated with \code{\link{extractB}} from the
#' fitted MARSS object.
#'
#' @return A single numeric numeric, the maximal amplification factor by which a
#' perturbation is amplified (relative to its initial size) before the system's
#' eventual equilibrium.
#' If
#' \eqn{A_{max} > 1}
#' , the system overreacts and departs from equilibrium.
#' If
#' \eqn{A_{max} \approx 1}
#' , the system is stable.
#'
#' @details
#'
#' \deqn{
#' A_{max} = \max_{t \ge 0}
#' \left(
#' \max_{x_0 \ne 0}
#' \frac{
#' \left\lVert e^{\, B^{\mathsf{T}} t}\, x_0 \right\rVert
#' }{
#' \left\lVert x_0 \right\rVert
#' }
#' \right)
#' }
#'
#' @seealso
#' [estar::extractB()]
#'
#' @references
#' Neubert, M. G., & Caswell, H. (1997). Alternatives to Resilience for Measuring the Responses of Ecological Systems to Perturbations. Ecology, 78(3), 653-665.
#'
#' Devin Incerti and Jeroen P Jansen (2021). hesim: Health Economic Simulation Modeling and Decision Analysis. URL: \doi{10.48550/arXiv.2102.09437}
#'
#' @example man/examples/eg_max_amp.R
#'
#' @export
max_amp <- function(B) {
  m <- Matrix::norm(hesim::expmat(B)[, , 1], type = "f")
  return(m)
}
