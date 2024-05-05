#' Calculate the persistence of a state variable inside a defined interval
#'
#' \code{persistence} returns the proportion of time the state
#' variable remained inside the interval defined by the baseline's
#' \eqn{\pm} sd. The proportion is calculated in relation to the time
#' period for which persistence should be calculated. ## V: actually tend to exclude persistence altogether from the package.
#'
#' @inheritParams univar_params
#'
#' @return a double, contained in \[0,1\]
#'
#' @details If the baseline is defined by the pre-disturbed values of the
#' state variable in the disturbed system (\code{bl = "db"}), this pre-disturbed
#' time period used as baseline (\code{bl_tf}) cannot overlap with the time period
#' for which the persistence is to be calculated (\code{metric_tf}), because of redundancy:
#' the values over \code{bl_tf} define the interval for which the values in \code{metric_tf} are
#' checked. If they are the same (or partly, if overlap is partial), the
#' returned value of persistence will be falsely higher.
#'
#' @examples
#' persistence(
#'   svdb_i = "statvar_db", tdb_i = "time", db_data = aquacomm_resps, bl = "db",
#'   bl_tf = c(1, 9), metric_tf = c(50, 100)
#' )
#' persistence(
#'   svdb_i = "statvar_db", tdb_i = "time", db_data = aquacomm_resps, bl = "db",
#'   bl_tf = c(1, 9), metric_tf = c(30, 100)
#' )
#' persistence(
#'   svdb_i = "statvar_db", tdb_i = "time", db_data = aquacomm_resps, bl = "input",
#'   metric_tf = c(50, 100), svbl_i = "statvar_bl", tbl_i = "time",
#'   bl_data = aquacomm_resps
#' )
#' persistence(
#'   svdb_i = "statvar_db", tdb_i = "time", db_data = aquacomm_resps, bl = "input",
#'   metric_tf = c(30, 100), svbl_i = "statvar_bl", tbl_i = "time",
#'   bl_data = aquacomm_resps
#' )
#' @export
persistence <-
  function(svdb_i,
           tdb_i,
           db_data = NULL,
           metric_tf,
           bl,
           bl_tf = NULL,
           svbl_i = NULL,
           tbl_i = NULL,
           bl_data = NULL,
           na_rm = TRUE) {
    dbts_df <- format_input(input = "db", svdb_i, tdb_i, db_data)

    if (bl == "input") {
      blts_df <- format_input(input = "bl", svbl_i, tbl_i, bl_data)
      names(blts_df)[which(names(blts_df) == "svbl_i")] <- "sv"
    } else {
      if (bl == "db") {
        if (max(bl_tf) > min(metric_tf)) {
          stop("Baseline overlaps with persistence period. Check Details.")
        }
        blts_df <-
          subset(dbts_df, tdb_i >= min(bl_tf) & tdb_i <= max(bl_tf))
        names(blts_df)[which(names(blts_df) == "svdb_i")] <- "sv"
      } else {
        stop("bl must be \"input\" or \"db\".")
      }
    }

    perst_zone <- list(
      mean_sv = mean(blts_df$sv, na.rm = na_rm),
      sd_sv = sd(blts_df$sv, na.rm = na_rm)
    )
    perst_zone$low_lim <- perst_zone$mean_sv - perst_zone$sd_sv
    perst_zone$high_lim <- perst_zone$mean_sv + perst_zone$sd_sv

    persistence_df <-
      subset(dbts_df, tdb_i >= min(metric_tf) & tdb_i <= max(metric_tf))
    persistence_df$persist <-
      sapply(persistence_df$svdb_i, function(x)
        all(x >= perst_zone$low_lim & x <= perst_zone$high_lim))
    persistence_agg <-
      aggregate(persistence_df$persist,
                by = list(persistence_df$persist),
                FUN = length)
    colnames(persistence_agg) <- c("persist", "n_p")

    persistence = persistence_agg$n_p[which(persistence_agg$persist== TRUE)]/sum(persistence_agg$n_p)

    ## necessary if all persist values are FALSE, and data frame ends up empty
    if (nrow(persistence_df) == 0) {
      persistence <- 0

    }
    return(persistence)
  }

