#' Calculate the maximal amplification of a community from its
#' community matrix.
#'
#'\code{max_amp} calculates the maximal amplification as the euclidean
#' norm of a community matrix $B$ (Neubert et al. 1996). We use the
#' \code{expmat} function of the \code{hesim} package to calculate
#' the exponential of the community matrix $B$, and then its
#' Euclidean norm.
#'
#' @param B a matrix, containing the interactions between the species
#' or functional groups in the community.
#' Can be calculated with \code{\link{extractB}} from the
#' fitted MARSS object.
#'
#' @return a numeric, the maximal amplification vector
#'
#' @example man/examples/eg_max_amp.R
#'
#' @export
max_amp <- function(B) {
  m <- Matrix::norm(hesim::expmat(B)[, , 1], type = "f")
  return(m)
}
