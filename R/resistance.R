#' Calculate the resistance of a state variable to disturbance
#'
#' \code{resistance} returns either the distance of a state variable to
#' a baseline value at a specified time point or a maximum distance between the
#' state variables in the disturbed system and the baseline over a specified
#' period. The distance can be calculated either as the absolute difference
#' between the state variables in the disturbed system and the baseline,
#' or as the log response ratio between these state variables.
#' See details on how to specify the values.
#'
#' @param res_mode A string stating whether the resistance should be calculated
#' as the log response ratio of the state variable in the disturbed system
#' compared to the baseline (\code{res_mode = "lrr"}) or the difference
#' (\code{res_mode = "diff"}) between the values of these state variables.
#' See details.
#' @param res_time A string stating whether resistance should be calculated at
#' a specific point in time (\code{res_time = "defined"}) or if it should be
#' taken as the maximal difference between the disturbed and baseline state
#' variables over a specified time period (\code{res_time = "max"}).
#' Time point and the time period are defined by \code{res_t} and
#' \code{res_tf}, respectively.
#' See details.
#' @param res_t An integer defining the time point when resistance should be
#' measured if \code{res_time = "defined"}.
#' @param res_tf A vector, specifying the time period for which the maximum
#' resistance should be looked for, if \code{res_time = "max"}.
#' @param comm_t an optional string with the name of the time variable in the community data. Only necessary when calculating maximal resistance.
#' @inheritParams univar_params
#'
#' @details If resistance is calculated at a specific time point, it is
#' conventionally the first time point after the disturbance.
#'
#' Even though it is possible to use a single data value as baseline
#' (by passing a double to \code{b_tf}), it is not recommended, because a
#' single value does not account for any variability in the system arising from,
#' for example, demographic or environmental stochasticity.
#'
#' @return A double, the resistance of the state variable to disturbance.
#'
#' @examples
#' resistance(
#'   vd_i = "statvar_db", td_i = "time", d_data = aquacomm_resps, b = "input",
#'   vb_i = "statvar_bl", tb_i = "time", b_data = aquacomm_resps,
#'   res_mode = "lrr", res_time = "defined", res_t = 12, type = "functional"
#' )
#' resistance(
#'   vd_i = "statvar_db", td_i = "time", d_data = aquacomm_resps, b = "input",
#'   vb_i = "statvar_bl", tb_i = "time", b_data = aquacomm_resps,
#'   type = "functional", res_mode = "diff", res_time = "defined", res_t = 12
#' )
#' resistance(
#'   vd_i = "statvar_db", td_i = "time", d_data = aquacomm_resps, b = "d",
#'   b_tf = 8, type = "functional", res_mode = "lrr",
#'   res_time = "defined", res_t = 12
#' )
#' resistance(
#'   vd_i = "statvar_db", td_i = "time", d_data = aquacomm_resps, b = "d",
#'   b_tf = 8, type = "functional", res_mode = "diff",
#'   res_time = "defined", res_t = 12
#' )
#' resistance(
#'   vd_i = "statvar_db", td_i = "time", d_data = aquacomm_resps, b = "input",
#'   vb_i = "statvar_bl", tb_i = "time", b_data = aquacomm_resps,
#'   type = "functional", res_mode = "lrr", res_time = "max", res_tf = c(12, 51)
#' )
#' resistance(
#'   vd_i = "statvar_db", td_i = "time", d_data = aquacomm_resps, b = "input",
#'   vb_i = "statvar_bl", tb_i = "time", b_data = aquacomm_resps,
#'   type = "functional", res_mode = "diff", res_time = "max", res_tf = c(12, 51)
#' )
#' resistance(
#'   vd_i = "statvar_db", td_i = "time", d_data = aquacomm_resps, b = "d",
#'   type = "functional", res_mode = "lrr", b_tf = 8, res_time = "max",
#'   res_tf = c(12, 51)
#' )
#' resistance(
#'   vd_i = "statvar_db", td_i = "time", d_data = aquacomm_resps, b = "d",
#'   type = "functional", res_mode = "lrr", b_tf = 8, res_time = "max",
#'   res_tf = c(12, 51)
#' )
#' @export
resistance <- function(type,
                       res_mode = NULL,
                       res_time = NULL,
                       res_t = NULL,
                       res_tf = NULL,
                       b = NULL,
                       b_tf = NULL,
                       vb_i = NULL,
                       tb_i = NULL,
                       b_data = NULL,
                       vd_i = NULL,
                       td_i = NULL,
                       d_data = NULL,
                       comm_b = NULL,
                       comm_d = NULL,
                       comm_t = NULL,
                       method = "bray",
                       binary = "FALSE",
                       na_rm = TRUE) {
  if (!(type %in% c("functional", "compositional"))) {
    stop("type must be \"functional\" or \"compositional\".")
  }

  if (type == "functional") {

    if (!(res_mode %in% c("lrr", "diff"))) {
      stop("res_mode must be \"lrr\" or \"diff\".")
    }

    if (b == "d" && !all(b_tf %in% d_data[[td_i]])) {
      stop("b_tf must be a time step in your data.")
    }

    if (!(res_time %in% c("defined", "max"))) {
      stop("res_time must be \"defined\" or \"max\".")
    }

    dts_df <- format_input("d", vd_i, td_i, d_data)

    if (b == "input") {
      bts_df <- format_input("b", vb_i, tb_i, b_data)

      res_df <- merge(
        data.frame("vd_i" = dts_df$vd_i, "t" = dts_df$td_i),
        data.frame("vb_i" = bts_df$vb_i, "t" = bts_df$tb_i)
      )
    } else {
      if (b == "d") {
        if (min(b_tf) == max(b_tf)) {
          warning(
            "You are using a single time point as baseline. Consider a time period, see Details."
          )
        }
        b <- summ_d2b(dts_df, b_tf, "mean", na_rm)
        res_df <- data.frame("t" = dts_df$td_i,
                             "vd_i" = dts_df$vd_i,
                             "vb_i" = b)
      } else {
        stop("b must be \"input\" or \"d\".")
      }
    }

    if (res_time == "defined") {
      if (!res_t %in% res_df$t) {
        stop("res_t must be a time step in both d_data and b_data (if b_data is used).")
      }
      res_df <- res_df[(res_df$t == res_t), ]
      if (identical(res_mode, "lrr")) {
        # Safety: log-ratio requires positive values
        if (any(res_df$vd_i <= 0 | res_df$vb_i <= 0, na.rm = TRUE)) {
          stop("log-ratio requires vd_i > 0 and vb_i > 0 in the filtered data.")
        }
        res_df$res <- log(res_df$vd_i / res_df$vb_i)   # natural log; use log10() if you prefer base-10
      } else if (identical(res_mode, "diff")) {
        res_df$res <- res_df$vd_i - res_df$vb_i
      } else {
        stop("res_mode must be 'lrr' or 'diff'.")
      }

      res <- res_df$res

    } else {
      res_df <- res_df[(res_df$t >= min(res_tf) &
                          res_df$t <= max(res_tf)), ]
      if (identical(res_mode, "lrr")) {
        # Safety: log-ratio requires positive values
        if (any(res_df$vd_i <= 0 | res_df$vb_i <= 0, na.rm = TRUE)) {
          stop("log-ratio requires vd_i > 0 and vb_i > 0.")
        }
        res_df$res <- log(res_df$vd_i / res_df$vb_i)   # natural log; use log10() if you prefer base-10
      } else if (identical(res_mode, "diff")) {
        res_df$res <- res_df$vd_i - res_df$vb_i
      } else {
        stop("res_mode must be 'lrr' or 'diff'.")
      }

      res <- res_df[which(abs(res_df$res) == max(abs(res_df$res), na.rm = na_rm)), "res"]
    }
  } else {

    if (res_time == "defined") {

      res_df <- rbind(comm_b, comm_d) |>
        (\(.) subset(., .[[comm_t]] == res_t))()

      res <- calc_dissim(res_df, comm_t, method, binary)

    } else {
      ## Combine disturbed and baseline data into a single one
      res_df <- rbind(comm_d, comm_b) |>
        (\(.) subset(., .[[comm_t]] >= min(res_tf) &
                       .[[comm_t]] <= max(res_tf)))()

      dissim <- unlist(calc_dissim(res_df, comm_t, method, binary), use.names = FALSE)

      res <- dissim[which(abs(dissim) == max(abs(dissim), na.rm = TRUE))]

    }
  }
  return(res)
}
