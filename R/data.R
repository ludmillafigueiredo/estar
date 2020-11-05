#' Time-series of mock state variable
#'
#' A dataset containing the time-series of a mock state variable
#' following a Uniform distribution.
#' Simplest possible example of dataset to be used during early
#' stages of development.
#'
#' @format A data frame with 100 rows and 2 variables:
#' \describe{
#'   \item{sv}{state variable}
#'   \item{t}{time}
#' }
#' @source \code{
#' set.seed(777)
# mock_svts <- tibble::tibble(sv = runif(n = 100, min = 50, max = 100), t = seq(1,100)) %>%
#   dplyr::mutate(sv = dplyr::case_when(
#   t >= 10 && t < 15 ~ runif(n = 1, min = 0, max = 30),
#   TRUE ~ sv))
#   usethis::use_data(mock_svts)
#'   }
"mock_svts"

#'
#' A dataset containing the baseline time-series of a mock state variable
#' following a Uniform distribution, limited to [0,0.5].
#' Simplest possible example of dataset to be used during early
#' stages of development.
#'
#' @format A data frame with 100 rows and 2 variables:
#' \describe{
#'   \item{sv}{state variable}
#'   \item{t}{time}
#' }
#' @source \code{
#' set.seed(777)
# mock_blts <- tibble::tibble(sv = runif(n = 100, min = 50, max = 100), t = seq(1,100))
#   usethis::use_data(mock_blts)
#'   }
"mock_blts"



