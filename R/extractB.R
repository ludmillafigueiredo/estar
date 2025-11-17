#' Extract the community matrix (B) estimated by a MARSS model.
#'
#' @param marss_res MARSS object returned by \code{\link[MARSS]{MARSS}}
#' @param states_names a string vector containing the names of species/groups
#' for which interactions were estimated.
#'
#' @return A named, Jacobian (square) matrix (B) of estimated interaction strengths
#' between the species/groups. The matrix is named after the strings in
#' \code{states_names}.
#'
#' @references
#' Holmes, E. E., Ward, E. J., Scheuerell, M. D., & Wills, K. (2024). MARSS: Multivariate Autoregressive State-Space Modeling (Version 3.11.9). \doi{10.32614/CRAN.package.MARSS}
#'
#' Holmes, E. E., Scheuerell, M. D., & Ward, E. J. (2024). Analysis of multivariate time-series using the MARSS package. Version 3.11.9. \doi{10.5281/zenodo.5781847}
#'
#' Holmes, E. E., Ward, E. J., & Wills, K. (2012). MARSS: Multivariate autoregressive state-space models for analyzing time-series data. The R Journal, 4(1), 30. \doi{10.32614/RJ-2012-002}
#'
#' @example man/examples/eg_extractB.R
#'
#' @export
extractB <- function(marss_res, states_names = NULL) {
  if (is.null(states_names)) {
    states_names <- 1:sqrt(length(stats::coef(marss_res)$B))
  }

  stats::coef(marss_res)$B |>
    matrix(
      nrow = length(states_names),
      ncol = length(states_names),
      byrow = FALSE,
      dimnames = list(states_names, states_names)
    )
}
