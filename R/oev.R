#' Calculate the overall ecological vulnerability of a community after disturbance.
#'
#' \code{oev} returns area under the curve of the absolute log-response-ratio (functional stability) or the dissimilarity (compositional stability) between the disturbed and baseline communities.
#'
#' @param response a string stating whether the stability metric should be
#' calculated using the log-response ratio between the values in the disturbed
#' system and the baseline (\code{response = "lrr"}) or using the state
#' variable values in the disturbed system alone (\code{response == "v"}).
#' @inheritParams common_params
#'
#' @return a double, the rate of recovery
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
