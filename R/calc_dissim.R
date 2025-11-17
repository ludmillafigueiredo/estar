#' Calculate dissimilarity
#'
#' @param df dataframe with "temporal community data": species abundances of the
#' baseline and disturbed communities over time.
#'
#' @inheritParams common_params
#'
#' @keywords internal
calc_dissim <- function(df, comm_t, method, binary){
  dissim <- df |>
    (\(.) split(., .[[comm_t]]))() |>
    lapply(\(.) .[ , !(names(.) == comm_t), drop = FALSE]) |>
    lapply(vegan::vegdist, method = method, binary = binary) |>
    lapply(\(.) ifelse(is.na(.[1]), NA, .)) |>
    lapply(\(.) unlist(.))

  return(dissim)
}
