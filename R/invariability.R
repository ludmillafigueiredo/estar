#' Calculate the invariability of state variable after disturbance.
#'
#' @description Return the inverse of coefficient of variation or
#' that of the standard-deviation of residuals of the linear model
#' derived for the ratio between response over time.
#'
#' @param svts_path Path to the state variable time series for which
#' invariability should be calculated.
#' @param variant String stating whether invariability should be calculated
#' as the inverse of the coefficient of variation of the state variable
#' \code{"cv"}, or as the residual of the linear model derived for the log
#' response ratio between the response and a baseline.
#' @param time_frame First and last time steps defining the time frame for which
#'   invariability should be calculated from the baseline should be looked for.
#' @param bl_path Path to the state variable time series to be used as baseline.

invariability <- function(svts_path, variant, time_frame, bl_path){
    
    svts_df <- read_csv(svts_path,
                        col_names = TRUE,
                        col_types = cols(sv = col_double(), t = col_integer()))

    if(variant == "cv"){

        invar <- 1/(sd(svts_df$sv)/mean(svts_df$sv))

        return(invar)
        
    }else{
        
        bl_df <- read_csv(bl_path,
                          col_names = TRUE,
                          col_types = cols(sv = col_double(), t = col_integer())) %>%
            rename(sv = sv_bl)

        invar_df <- inner_join(svts_df, bl_df) %>%
            filter(t %in% seq(time_frame)) %>%
            mutate(lrr = log(sv/sv_bl))

        invar <- 1/sd(lm(lrr~t)$residuals)
        
        return(invar)
        
    }
}
