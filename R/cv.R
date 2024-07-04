#' Calculate coefficient of variation of a vector
#'
#' @param vct a numerical vector, for which coefficient of variation should be
#' calculated.
#' @param na_rm a logical indicating whether NA values should be removed before
#' processing.
#'
#' @export
cv <- function(vct, na_rm) {
  stats::sd(vct, na.rm = na_rm) / mean(vct, na.rm = na_rm)
}
