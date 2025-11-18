#' Calculate the overall ecological vulnerability of a community after disturbance
#'
#' \code{oev} returns the overall ecological vulnerability \eqn{OEV}
#' , calculated as the area under the curve ( \eqn{AUC} ) of the absolute
#' log-response-ratio (functional stability) or the dissimilarity
#' (compositional stability) between the disturbed and baseline communities
#' .
#' The area under the curve is calculated through trapezoidal integration.
#'
#' @param response a string stating whether the stability metric should be
#' calculated using the log-response ratio between the values in the disturbed
#' system and the baseline (\code{response = "lrr"}) or using the state
#' variable values in the disturbed system alone (\code{response == "v"}).
#' @inheritParams common_params
#'
#' @return A single numeric value, the overall ecological vulnerability.
#' \eqn{OEV \ge 0}
#' . The higher the value, the less stable the system.
#'
#' @details
#' The overall ecosystem variability (OEV) is defined as the area under the
#' curve (AUC) of the system's response through time.
#' For functional stability, the response is the log response ratio between
#' the state variable’s value in the disturbed ( \eqn{v_d} ) and in the baseline
#' systems ( \eqn{v_b} or \eqn{v_p} if the baseline is pre-disturbance values).
#' For compositional stability, the response is the dissimilarity between the
#' disturbed ( \eqn{C_d} ) and baseline ( \eqn{C_b} ) communities.
#' Therefore,
#'
#' \eqn{
#' \mathrm{OEV} = \mathrm{AUC}\!\left(
#'   \left\lvert \log\!\left( \frac{v_d(t)}{v_b(t)} \right) \right\rvert,\, t
#' \right)
#' }
#'
#' or
#'
#' \eqn{
#' \mathrm{OEV} = \mathrm{AUC}\!\left(
#'   \left\lvert \log\!\left( \frac{v_d(t)}{v_p(t)} \right) \right\rvert,\, t
#' \right)
#' }
#'
#' or
#'
#' \eqn{
#' \mathrm{OEV} = \mathrm{AUC}\!\left(
#'   \mathrm{dissim}\!\left( \frac{C_d(t)}{C_b(t)} \right),\, t
#' \right)
#' }
#'
#' where the area under the curve is defined as
#'
#' \eqn{
#' \mathrm{AUC}(y, t)
#'   = \sum_{i = 1}^{n - 1}
#'     \frac{\left(t_{i+1} - t_i\right)\left(y_i + y_{i+1}\right)}{2}
#' }
#' (trapezoidal integration).
#'
#' @references Urrutia-Cordero, P., Langenheder, S., Striebel, M., Angeler, D. G., Bertilsson, S., Eklöv, P., Hansson, L.-A., Kelpsiene, E., Laudon, H., Lundgren, M., Parkefelt, L., Donohue, I., & Hillebrand, H. (2022). Integrating multiple dimensions of ecological stability into a vulnerability framework. Journal of Ecology, 110(2), 374–386. \doi{10.1111/1365-2745.13804}

#'
#' @examples
#' oev(
#'   type = "functional", response = "lrr", metric_tf = c(0.14, 56), vd_i = "statvar_db",
#'   td_i = "time", d_data = aquacomm_resps, vb_i = "statvar_bl", tb_i = "time",
#'   b_data = aquacomm_resps
#' )
#' oev(
#'   type = "compositional", metric_tf = c(0.14, 56), comm_d = comm_dist,
#'   comm_b = comm_base, comm_t = "time"
#' )
#' @export
oev <- function(type,
                metric_tf,
                response,
                vd_i,
                td_i,
                d_data = NULL,
                vb_i = NULL,
                tb_i = NULL,
                b_data = NULL,
                comm_d = NULL,
                comm_b = NULL,
                comm_t = NULL,
                method = "bray",
                binary = "FALSE",
                na_rm = TRUE){

  if (type == "functional"){

    dts_df <- format_input("d", vd_i, td_i, d_data)

    oev_df <- sort_response(response, dts_df, vb_i, tb_i, b_data) |>
      (\(.) .[.[["t"]] >= min(metric_tf) &
                   .[["t"]] <= max(metric_tf),])()

    oev_df$log <- log(oev_df$vd_i / oev_df$vb_i)

    auc <- abs(sum(diff(oev_df$t) * zoo::rollmean(oev_df$response, 2), na.rm = TRUE))

  } else{

    common_t <- intersect(comm_d[[comm_t]], comm_b[[comm_t]])

    comm_b_sub <- comm_b[comm_b[[comm_t]] %in% common_t, ]
    comm_d_sub <- comm_d[comm_d[[comm_t]] %in% common_t, ]

    base_df <- rbind(comm_d_sub, comm_b_sub) |>
      (\(.) subset(., .[[comm_t]] >= min(metric_tf) &
                     .[[comm_t]] <= max(metric_tf)))()

    dissim <- calc_dissim(base_df, comm_t, method, binary)

    auc <- sum(diff(as.numeric(names(dissim))) * zoo::rollmean(unlist(dissim, use.names = FALSE), 2))

  }

  return(auc)

}
