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

    svts_df <- readr::read_csv(svts_path,  ## RV: important: pay attention to the namespace! + try to use as few dependencies as possible, so I would here just use base function read.csv
                        col_names = TRUE,
                        col_types = readr::cols(sv = readr::col_double(), t = readr::col_integer()))  ## RV: this necessitates that the user has named the variables as
                                                                                 ## sv and time as t. I think I would rather not include path as an argument
                                                                                 ## and instead have directly the vector of sv as an input (+ time vector)
                                                                                 ## I think this will allow for more flexibility, as the user can call his variable the
                                                                                 ## way s/he likes and s/he just has ot supply it as an argument.
                                                                                 ## I do not think that it is really needed that the data are stored externally,
                                                                                 ## the user should be able to read-in the data on his own + prepare if needed


    if(variant == "cv"){

        invar <- 1/(sd(svts_df$sv)/mean(svts_df$sv))  ## VR: this implies that the whole time series is used to caclulate mean and SD. But it is possible that the
        ## disturbance is applied at timestep = 5 (or whatever, X), so that the first X time steps have to be discarded when calculating the invariability
        ## + what about current formulations if there are NAs in the time series???

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
