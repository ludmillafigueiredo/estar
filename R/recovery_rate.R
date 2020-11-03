#' Calculate the extent of recovery.
#'
#' @description Returns the log-ratio response of a state variable in relation to a baseline or between two time steps.
#'
#' @details
#' Read in a state variable time-series (\code{svts_df}) from the file path
#' \code{svts_path}.
#' If \code{bl = TRUE}, read the baseline time-series from \code{bl_path},
#' an calculate the log-ratio response at \code{t = time_frame}.
#' If \code{bl = FALSE}, calculate the log-ratio response between that values of state variable at the time steps listed in \code{time_frame}.
#' 
#' @param svts_path Path to the state variable time series for which resistance should be calculated.
#' @param bl Boolean determining whether recovery is calculated in relation to a baseline time series or to a point in the state variable time series.
#' @param slope_lm Boolean determining whether recovery rate is calculated as the slope of a linear model \code{slope_lm = TRUE} or between two points in the time series.
#' @param time_frame If \code{bl = TRUE}, the time step at which extent of recovery should be measured in the baseline and in the response. If \code{bl =FALSE}, the first and last time steps defining the time frame between which the extent of recovery should be calculated for.

recovery_rate <- function(svts_path, bl, bl_path, time_frame){
    
    svts_df <- read_csv(svts_path,
                        col_names = TRUE,
                        col_types = cols(sv = col_double(), t = col_integer()))
    
    if(bl == TRUE){

        blts_df <- read_csv(bl_path,
                            col_names = TRUE,
                            col_types = cols(sv = col_double(), t = col_integer())) %>%
            rename(sv_bl = sv)
        
        base_df <- inner_join(blts_df, svts_df) %>%
            mutate(extent = log(sv/sv_bl)) %>%
            select(t, extent)

    }else{

        sv_predist <- filter(svts_df, t == time_frame)
        
        base_df <- svts_df %>%
            mutate(extent = log(sv/sv_predist))
    }

    if(slope_lm == TRUE){
        
        lm_df <- base_df %>%
            filter(t %in% seq(min(time_frame), max(time_frame)))

        rate_lm <- lm(extent~t, data = lm_df)

        return(rate_lm$coefficients[["t"]])
        
    }else{

        rate_df <- bl_df %>%
            filter(t %in% time_frame) %>%
            arrange(t) %>%
            mutate(lim = case_when(
                       time_frame == min(time_frame) ~ "min_t",
                       time_frame == max(time_frame) ~ "max_t")) %>%
            select(lim, sv) %>%
            pivot_wider(names_from = lim, values_from = sv) %>%
            mutate(rate = (max_t - min_t)/(max(time_frame)-min(time_frame)))
        
        return(rate_df$rate)
        
    }
}
