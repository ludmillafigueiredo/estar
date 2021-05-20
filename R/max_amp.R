#' Calculate the maximal amplification of a community from its community matrix.

#'\code{max_amp} calculates the maximal amplification as the euclidean norm of a community matrix 4B$ (Neubert et al. 1996). We use the \code{expmat} function of the \code{hesim} package to calculate the exponential of the community matrix $B$, and then it's Euclidean norm.
#'
#' @param B a matrix, containing the species or functional groups in the community. Can be calculated with \code{\link{get_B}}.
#'
#' @return a numeric, the maximal amplification vector
#'
#' @export
max_amp <- function(B){
  Matrix::norm(hesim::expmat(B)[,,1], type = "f")
}
