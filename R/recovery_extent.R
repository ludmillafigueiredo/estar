#' Calculate the extent of recovery after disturbance
#'
#' \code{recovery_extent} ( \eqn{R_e} ) calculates how close a state variable is
#' to its baseline value at a time point specified by the user
#' (usually after recovery has taken place). For functional stability,
#' the  distance can be calculated as the log-response ratio or as the
#' difference between the state variables in a disturbed
#' time-series and the baseline. The baseline can be
#' \itemize{
#' \item a value at time \code{t_rec} of the baseline time-series
#' \code{b_data} (if \code{b = "input"}).
#' \item pre-disturbance values of the state variable in the disturbed system
#' over a period defined by \code{b_tf} (if \code{b = "d"}). In that case,
#' the state variable is summarized as the mean or median (\code{summ_mode}).
#' }
#' For community stability, the distance is calculated as the dissimilarity
#' between the disturbed and baseline communities.
#'
#' @param response a string stating whether the stability metric should be
#' calculated using the log-response ratio between the values in the disturbed
#' system and the baseline (\code{response = "lrr"}) or using the state
#' variable values in the disturbed system alone.
#' @param summ_mode A string, stating whether the baseline should be summarized as
#' the mean (\code{summ_mode = "mean"}) or the median (\code{summ_mode = "median"}).
#' Defaults to "mean".
#' @param t_rec An integer, time point at which the extent of recovery should be
#' calculated.
#' @inheritParams common_params
#'
#' @return A numeric, the extent of recovery. Maximum (functional and
#' compositional) recovery at 0. For functional stability, smaller or higher
#' values indicate under- or overcompensation, respectively. For compositional
#' stability using the Bray-Curtis index (default),
#' \eqn{0 \le R_e \le 1}
#' .
#' The higher the index, the further apart the communities are after recovery,
#' and thus, the lower the recovery is.
#'
#' @details
#' For functional stability, the log-response ratio or difference between the
#' state variable in the disturbed systems
#' \eqn{v_d}
#' and in the baseline
#' ( \eqn{v_b} or \eqn{v_p}
#' if the baseline is pre-disturbance values) measured on the user-defined
#' time step when the recovery is assumed to have taken place. Therefore,
#' \eqn{R_e = \log\!\left(\frac{v_d(t)}{v_b(t)}\right)}
#' , or
#' \eqn{R_e = \log\!\left(\frac{v_d(t)}{v_p(t)}\right)}
#' , or
#' \eqn{R_e = \lvert v_d(t) - v_b(t) \rvert}
#' , or
#' \eqn{R_e = \lvert v_d(t) - v_p(t) \rvert}
#' .
#'
#' For community stability, the dissimilarity between the disturbed
#' ( \eqn{C_d} )
#' and baseline
#' ( \eqn{C_b} )
#' communities
#' \eqn{R_e = \mathrm{dissim}\!\left(\frac{C_d(t)}{C_b(t)}\right)}
#' .
#'
#' Even though it is possible to use a single data value as baseline, it is not
#' recommended, because a single value does not account for any variability in
#' the system arising from, for example, demographic or environmental
#' stochasticity.
#'
#' @examples
#' recovery_extent(
#'   vd_i = "statvar_db", td_i = "time", d_data = aquacomm_resps,
#'   response = "lrr", b = "input", t_rec = 42, vb_i = "statvar_bl",
#'   tb_i = "time", b_data = aquacomm_resps, type = "functional"
#' )
#' recovery_extent(
#'   vd_i = "statvar_db", td_i = "time", d_data = aquacomm_resps,
#'   response = "diff", b = "input", t_rec = 42, vb_i = "statvar_bl",
#'   tb_i = "time", b_data = aquacomm_resps, type = "functional"
#' )
#' recovery_extent(
#'   vd_i = "statvar_db", td_i = "time", d_data = aquacomm_resps,
#'   response = "lrr", b = "d", t_rec = 42, b_tf = 8, type = "functional"
#' )
#' recovery_extent(
#'   vd_i = "statvar_db", td_i = "time", d_data = aquacomm_resps,
#'   response = "lrr", b = "d", t_rec = 42, b_tf = c(5, 10), type = "functional"
#' )
#' recovery_extent(
#'   vd_i = "statvar_db", td_i = "time", d_data = aquacomm_resps,
#'   response = "lrr", type = "functional",
#'   b = "d", t_rec = 42, b_tf = c(5, 10), summ_mode = "median"
#' )
#' recovery_extent(
#'   type = "compositional", t_rec = 28, comm_d = comm_dist,
#'   comm_b = comm_base, comm_t = "time"
#' )
#' @export
recovery_extent <- function(type,
                            response = NULL,
                            t_rec,
                            summ_mode = "mean",
                            b = NULL,
                            b_tf = NULL,
                            vd_i = NULL,
                            td_i = NULL,
                            d_data = NULL,
                            vb_i = NULL,
                            tb_i = NULL,
                            b_data = NULL,
                            comm_d = NULL,
                            comm_b = NULL,
                            comm_t = NULL,
                            method = "bray",
                            binary = "FALSE",
                            na_rm = TRUE) {
  if (!(type %in% c("functional", "compositional"))) {
    stop("type must be \"functional\" or \"compositional\".")
  }

  if (type == "functional") {

    dts_df <- format_input("d", vd_i, td_i, d_data)

    if (b == "input") {
      bts_df <- format_input("b", vb_i, tb_i, b_data)

      extent_df <- merge(
        data.frame("vd_i" = dts_df$vd_i, "t" = dts_df$td_i),
        data.frame("vb_i" = bts_df$vb_i, "t" = bts_df$tb_i)
      )

      ifelse(!(t_rec %in% extent_df$t),
             stop("Choose a t_rec for which you have input data."),
             extent_df <- extent_df[extent_df$t == t_rec, ])
    } else {
      if (b == "d") {
        if (min(b_tf) == max(b_tf)) {
          warning("You are using a single point as baseline. Consider an interval, see Details.")
        }
        b <- summ_d2b(dts_df, b_tf, summ_mode, na_rm)
        extent_df <- dts_df[dts_df$td_i == t_rec, ]
        extent_df$vb_i <- b
      } else {
        stop("b must be \"input\" or \"d\".")
      }
    }

    if (response == "lrr") {

      extent_df$extent <- log(extent_df$vd_i / extent_df$vb_i)
    } else if (response == "diff") {
      extent_df$extent <- extent_df$vd_i - extent_df$vb_i
    } else {
      stop("response must be \"lrr\" or \"diff\"")
    }

    return(extent_df$extent)

  } else {

    rec_df <- rbind(comm_b, comm_d) |>
      (\(.) .[.[[comm_t]] == t_rec, ])()

    dissim <- calc_dissim(rec_df, comm_t, method, binary)

    return(unlist(dissim, use.names = FALSE))
  }
}
