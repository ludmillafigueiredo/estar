#' Calculate the invariability of a state variable after disturbance
#'
#' \code{invariability} returns the temporal invariability
#' \eqn{I}
#' of a system following disturbance.
#' Invariability can be calculated using the post-disturbance values of the
#' state variable in the disturbed system as the response, or the log-response
#' ratio of the state variable in the disturbed system compared to the baseline.
#' Two variants of invariability can be calculated: as the inverse of the
#' coefficient of variation of the system's response, or the inverse of the
#' standard deviation of residuals of the linear model that uses the time
#' as the predictor of the system's response.
#'
#' @param mode A string stating which variant of invariability should be
#' calculated, the one based on the coefficient of variation of the state
#' variable \code{mode = "cv"}, or the one based on fitting the linear
#' model \code{"lm_res"}.
#' @inheritParams common_params
#'
#' @return A single numeric, the invariability ( \eqn{I} ) value. The larger in
#' magnitude
#' \eqn{I}
#' is, the higher the stability, since the variation
#' around the trend is lower.
#'
#' @details
#' Instability can be calculated as the coefficient fo variation of the
#' system:
#'
#' - For functional stability, the response is the log response ratio between
#' the state variable’s value in the disturbed ( \eqn{v_d} ) and in the baseline
#' systems ( \eqn{v_b} or \eqn{v_p} if the baseline is pre-disturbance values)
#' or the state variable’s value in the disturbed system itself. Therefore,
#' \eqn{I = \mathrm{CV}\!\left( \log\!\left( \frac{v_d}{v_b} \right) \right)^{-1}}
#' , or
#' \eqn{I = \mathrm{CV}\!\left( \log\!\left( \frac{v_d}{v_p} \right) \right)^{-1}}
#' , or
#' \eqn{I = \mathrm{CV}(v_d)^{-1}}
#' .
#'
#' - For compositional stability, the response is the dissimilarity between the
#' disturbed ( \eqn{C_d} ) and baseline ( \eqn{C_b} ) communities:
#' \eqn{I = \mathrm{CV}\!\left(\mathrm{dissim}\!\left( \frac{C_d}{C_b} \right) \right)^{-1} }
#'
#' Alternatively, instability can be calculated as inverse of the standard
#' deviation of residuals of the linear model where the response
#' (same as above) is predicted by time, whereby:
#' \eqn{I = \sigma(\varepsilon)^{-1}}
#' , from
#' \eqn{y = \alpha + R_r\, t + \varepsilon}
#' , where
#' \eqn{
#' y \in \left\{
#'   \log\!\left(\frac{v_d}{v_b} \right),
#'   \log\!\left(\frac{v_d}{v_p} \right),
#'   v_d,
#'   \mathrm{dissim}\!\left( \frac{C_d}{C_b} \right)
#' \right\}
#' }
#' ,
#' \eqn{\alpha}
#' is the intercept, and
#' \eqn{R_r}
#' is the recovery rate.
#'
#' @examples
#' invariability(
#'   vd_i = "statvar_db", td_i = "time", response = "v", mode = "cv",
#'   metric_tf = c(11, 50), d_data = aquacomm_resps, type = "functional"
#' )
#' invariability(
#'   vd_i = aquacomm_resps$statvar_db, td_i = aquacomm_resps$time,
#'   response = "v", mode = "cv", metric_tf = c(11, 50), type = "functional"
#' )
#' invariability(
#'   vd_i = "statvar_db", td_i = "time", response = "lrr", mode = "lm_res",
#'   metric_tf = c(11, 50), d_data = aquacomm_resps, vb_i = "statvar_bl",
#'   tb_i = "time", b_data = aquacomm_resps, type = "functional"
#' )
#' invariability(
#'   vd_i = aquacomm_resps$statvar_db, td_i = aquacomm_resps$time,
#'   response = "lrr", type = "functional",
#'   metric_tf = c(11, 50), mode = "lm_res", vb_i = aquacomm_resps$statvar_bl,
#'   tb_i = aquacomm_resps$time
#' )
#' invariability(
#'   type = "compositional", metric_tf = c(0.14, 56), comm_d = comm_dist,
#'   comm_b = comm_base, comm_t = "time"
#' )
#'
#' @export
invariability <- function(type,
                          mode = NULL,
                          response = NULL,
                          metric_tf,
                          vd_i = NULL,
                          td_i = NULL,
                          d_data = NULL,
                          vb_i = NULL,
                          tb_i = NULL,
                          b_data = NULL,
                          comm_b = NULL,
                          comm_d = NULL,
                          comm_t = NULL,
                          method = "bray",
                          binary = "FALSE",
                          na_rm = TRUE) {
  if (type == "functional") {

    dts_df <- format_input("d", vd_i, td_i, d_data)

    invar_df <- sort_response(response, dts_df, vb_i, tb_i, b_data)
    invar_df <- invar_df[invar_df$t >= min(metric_tf) &
                           invar_df$t <= max(metric_tf), ]

    if (any(is.na(invar_df$response))) {
      warning("NAs detected among the entries of the state variable")

      if (sum(!is.na(invar_df$response)) < 10) {
        warning("Less than 10 data points are available for measuring invariability.")
      }
    }

    if (mode == "cv") {
      invar <- 1 / cv(invar_df$response, na_rm = na_rm)
      return(invar)
    } else if (mode == "lm_res") {
      invar <- 1 / stats::sd(stats::lm(invar_df$response ~ invar_df$t)$residuals)
      return(invar)
    } else {
      stop("'mode' argument must be \"cv\" or \"lm_res\"")
    }

  } else {

    invar_df <- rbind(comm_d, comm_b) |>
      (\(.) subset(., .[[comm_t]] >= min(metric_tf) &
                     .[[comm_t]] <= max(metric_tf)))()

    dissim <- calc_dissim(invar_df, comm_t, method, binary)

    invar <- 1 / stats::sd(stats::lm(unlist(dissim, use.names = FALSE) ~ as.numeric(names(dissim)))$residuals)

    return(invar)
  }

}
