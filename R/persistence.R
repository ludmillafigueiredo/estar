#' Calculate the persistence of a state variable over a defined
#' time interval
#'
#' \code{persistence} returns the proportion of time the state
#' variable remained inside the interval defined by one baseline's
#' \eqn{\pm} sd from the baseline's mean (functional stability) or
#' the user (compositional stability). The proportion is calculated in
#' relation to the time period (\code{metric_tf}) defined by the user.
#'
#' @param low_lim minimal dissimilarity value the user expects for a persistent
#' community
#' @param high_lim maximal dissimilarity value the user expects for a persistent
#' community
#' @inheritParams univar_params
#'
#' @return a double, contained in \[0,1\]
#'
#' @details If the baseline is defined by the pre-disturbed values of the
#' state variable in the disturbed system (\code{b = "d"}), this pre-disturbed
#' time period used as baseline (\code{b_tf}) cannot overlap with the time period
#' for which the persistence is to be calculated (\code{metric_tf}), because of redundancy:
#' the values over \code{b_tf} define the interval for which the values in \code{metric_tf} are
#' checked. If they are the same (or partly, if overlap is partial), the
#' returned value of persistence will be falsely higher.
#'
#' @examples
#' persistence(
#'   type = "functional", vd_i = "statvar_db", td_i = "time", d_data = aquacomm_resps, b = "d",
#'   b_tf = c(1, 9), metric_tf = c(28, 56)
#' )
#' persistence(
#'   type = "functional", vd_i = "statvar_db", td_i = "time", d_data = aquacomm_resps, b = "d",
#'   b_tf = c(1, 9), metric_tf = c(28, 56)
#' )
#' persistence(
#'   type = "functional", vd_i = "statvar_db", td_i = "time", d_data = aquacomm_resps, b = "input",
#'   metric_tf = c(28, 56), vb_i = "statvar_bl", tb_i = "time",
#'   b_data = aquacomm_resps
#' )
#' persistence(
#'   type = "functional", vd_i = "statvar_db", td_i = "time", d_data = aquacomm_resps, b = "input",
#'   metric_tf = c(28, 56), vb_i = "statvar_bl", tb_i = "time",
#'   b_data = aquacomm_resps
#' )
#' @export
persistence <-
  function(type,
           metric_tf,
           b,
           b_tf = NULL,
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
           low_lim = NULL,
           high_lim = NULL,
           na_rm = TRUE) {

    if (type == "functional"){
      dts_df <- format_input("d", vd_i, td_i, d_data)

      if (b == "input") {
        bts_df <- format_input("b", vb_i, tb_i, b_data)
        names(bts_df)[which(names(bts_df) == "vb_i")] <- "v"
      } else {
        if (b == "d") {
          if (max(b_tf) > min(metric_tf)) {
            stop("Baseline overlaps with persistence period. Check Details.")
          }
          bts_df <-
            subset(dts_df, td_i >= min(b_tf) & td_i <= max(b_tf))
          names(bts_df)[which(names(bts_df) == "vd_i")] <- "v"
        } else {
          stop("b must be \"input\" or \"d\".")
        }
      }

      perst_zone <- list(
        mean_v = mean(bts_df$v, na.rm = na_rm),
        sd_v = stats::sd(bts_df$v, na.rm = na_rm)
      )
      perst_zone$low_lim <- perst_zone$mean_v - perst_zone$sd_v
      perst_zone$high_lim <- perst_zone$mean_v + perst_zone$sd_v

      persistence_df <-
        subset(dts_df, td_i >= min(metric_tf) &
                 td_i <= max(metric_tf))
      persistence_df$persist <-
        sapply(persistence_df$vd_i, function(x)
          all(x >= perst_zone$low_lim & x <= perst_zone$high_lim))
      persistence_agg <-
        stats::aggregate(persistence_df$persist,
                         by = list(persistence_df$persist),
                         FUN = length)
      colnames(persistence_agg) <- c("persist", "n_p")

      persistence = persistence_agg$n_p[which(persistence_agg$persist == TRUE)] /
        sum(persistence_agg$n_p)

      ## necessary if all persist values are FALSE, and data frame ends up empty
      if (is.na(persistence[1])) {
        persistence <- 0
      }
    } else {

      common_t <- intersect(comm_d[[comm_t]], comm_b[[comm_t]])

      # filter community for common time steps
      comm_b_sub <- comm_b[comm_b[[comm_t]] %in% common_t, ]
      comm_d_sub <- comm_d[comm_d[[comm_t]] %in% common_t, ]

      base_df <- rbind(comm_d_sub, comm_b_sub) |>
        (\(.) .[.[[comm_t]] >= min(metric_tf) &
                  .[[comm_t]] <= max(metric_tf), ])()

      dissim <- calc_dissim(base_df, comm_t, method, binary)

      persist <-
        sapply(unlist(dissim, use.names = FALSE), function(x)
          all(x >= low_lim & x <= high_lim))
      persistence_agg <-
        stats::aggregate(persist,
                         by = list(persist),
                         FUN = length)
      colnames(persistence_agg) <- c("persist", "n_p")

      persistence = persistence_agg$n_p[which(persistence_agg$persist == TRUE)] /
        sum(persistence_agg$n_p)

      ## necessary if all persist values are FALSE, and data frame ends up empty
      if (is.na(persistence[1])) {
        persistence <- 0
      }

    }

    return(persistence)
  }
