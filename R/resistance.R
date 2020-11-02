#' Calculate the resistance of a state variable to disturbance
#'
#' @description Returns the log-ratio response of a state variable.
#'
#' @details
#' Read in a state variable time-series (\code{sv_df}) from the file path
#' \code{svts_path}.
#' If \code{bl = "time_series"}, read the baseline time-series from \code{bl_path},
#' otherwise, the baseline is the state variable at \code{t = t_bl}.
#' According to \code{res__time}, resistance can be calculated at the first
#' time step after disturbance (\code{t_d + 1}) or as the maximal deviation
#' from baseline inside a time frame defined by \code{dev_frame}.
#'
#' @param svts_path Path to the state variable time series for which resistance should be calculated.
#' @param bl String stating the baseline in relation to which resistance
#'   should be calculated.
#' @param bl_path Path to the state variable time series to be used as baseline.
#' @param t_bl User-defined time step that should be used as baseline.
#' @param res_time String stating how to select the time step at which resistance
#'   should be calculated.
#' @param t_d Time step of disturbance.
#' @param dev_frame First and last time steps defining the time frame for which
#'   the largest deviation from the baseline should be looked for.
#' @return The log-ratio response between \code{sv} and \code{sv_bl}.

resistance <- function(svts_path, bl, bl_path, t_bl, res_time, t_d, dev_frame){

    svts_df <- read_csv(tseries,
                        col_names = TRUE,
                        col_types = cols(sv = col_double(), t = col_integer()))

    if(bl == "time_series"){
        bl_df <- read_csv(bl_path,
                          col_names = TRUE,
                          col_types = cols(sv = col_double(), t = col_integer())) %>%
            rename(sv = sv_bl)

        if(res_time == "defined"){
            resistance_df = inner_join(bl_df, svts_df) %>%
                filter(t == t_d + 1) %>%
                mutate(lrr = log(sv/sv_bl))

            return(resistance_df$lrr)

        }else{
            resistance_df = inner_join(bl_df, svts_df) %>%
                ## reduce length to the interval of search defined by the user
                filter(t %in% seq(dev_frame)) %>%
                mutate(lrr = log(sv/sv_bl)) %>%
                filter(max(lrr))

            return(resistance_df$lrr)

        }
    }else{
        bl <- svts_df %>%
            filter(t == t_bl)

        if(res_time == "defined"){
            resistance_df <- svts_df %>%
                filter(t == t_d + 1) %>%
                mutate(lrr = log(sv/bl$sv_bl))

            return(resistance_df$lrr)

        }else{
            resistance_df <- svts_df %>%
                filter(t %in% seq(dev_frame)) %>%
                mutate(lrr = log(sv/bl$sv_bl)) %>%
                filter(max(lrr))

            return(resistance_df$lrr)

        }
    }
}
