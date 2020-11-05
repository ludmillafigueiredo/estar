#' Time-series of mock state variable
#'
#' A dataset containing the time-series of a mock state variable
#' following Uniform distributions.
#' Disturbance is mocked at time step 10: until time-step 15 values drop below 30,
#' they rise to [30,50] until time step 50, and then get back into the "stable"
#' range [50,100].
#' Simplest possible example of dataset to be used during early
#' stages of development.
#'
#' @format A data frame with 100 rows and 2 variables:
#' \describe{
#'   \item{stat_var}{state variable column, intentionally named different from the function argument \code{sv} to be distinguished and make examples easier to understand}
#'   \item{time}{time column, intentionally named different from the function argument \code{sv} to be distinguished and make examples easier to understand}
#' }
#' @source \code{
#' set.seed(777)
#'mock_svts <- tibble::tibble(stat_var = runif(n = 100, min = 50, max = 100), time = seq(1,100)) %>%
#'   dplyr::mutate(stat_var = dplyr::case_when(
#'   time >= 10 && time < 15 ~ runif(n = 1, min = 0, max = 30),
#'   time >= 15 && time < 50 ~ runif(n = 1, min = 30, max = 50),
#'   TRUE ~ stat_var))
#'   usethis::use_data(mock_svts)
#'   }
"mock_svts"

#' Baseline time-series of mock state variable
#' 
#' A dataset containing the baseline time-series of a mock state variable
#' following a Uniform distribution, limited to [50,100].
#' Simplest possible example of dataset to be used during early
#' stages of development.
#'
#' @format A data frame with 100 rows and 2 variables:
#' \describe{
#'   \item{stat_var}{state variable column, intentionally named different from the function argument \code{sv} to be distinguished and make examples easier to understand}
#'   \item{time}{time column, intentionally named different from the function argument \code{sv} to be distinguished and make examples easier to understand}
#' }
#' @source \code{
#' set.seed(777)
#  mock_blts <- tibble::tibble(sv = runif(n = 100, min = 50, max = 100), t = seq(1,100))
#   usethis::use_data(mock_blts)
#'   }
"mock_blts"



