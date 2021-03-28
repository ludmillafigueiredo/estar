#' Build a design matrix with equal number of observations per state process
#'
#' @param reps a numeric, the number of replicates per state process
#' @param m a numeric, the number of state processes
#'
#' @return a numeric (design) matrix
#'
#' @export
build_Zreps  <- function(reps, m){
    ## n = reps*m
    Z <- matrix(list(0), reps*m, m)
    for(m_i in seq(0,m-1)){
        Z[(reps*m_i+1):(reps*m_i+1+(reps-1)), m_i+1] = 1
    }
    return(Z)
}
