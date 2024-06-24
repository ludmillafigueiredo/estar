#' Calculate the intrinsic stochastic invariability of a
#' community from its community matrix.
#'
#'\code{stoch_var} calculates the intrinsic stochastic
#'invariability - a theoretical equivalent of the
#'univariate measure of invariability (Arnoldi, Loreau,
#'and Haegeman 2016).
#'
#' @param B a matrix, containing the interactions between the species
#' or functional groups in the community. Can be calculated
#' with \code{\link{extractB}} from the fitted MARSS object.
#' VIK: please add an example
#'
#' @export
stoch_var <- function(B){
  id <- diag(nrow(B))
  kron_sum <- Matrix::kronecker(B, id) + Matrix::kronecker(id, B)
  Nu_s <- norm(solve(kron_sum), type ='2')
  I_s <- 1/2*Nu_s
}
