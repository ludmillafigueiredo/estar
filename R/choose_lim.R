choose_lim <- function(svr_c, lim_pos, lim_ref) {
    if (lim_pos == "u") {
      svr_c >= lim_ref
    } else {
      if (lim_pos == "l") {
        svr_c <= lim_ref
      } else {
        stop("lim_pos should be \"u\" or \"l\".")
      }
    }
  }
