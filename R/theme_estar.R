#' Custom theme for internal use to demo estar
#'
#' @keywords internal
#'
#' @importFrom ggplot2 theme_minimal theme element_text rel element_rect %+replace%
theme_estar <- function(){
  theme_minimal(base_size = 8)%+replace%
    #cowplot::theme_minimal_hgrid(12)%+replace%
    theme(
      axis.text.y = element_text(size = rel(0.85)),
      axis.text.x = element_text(size = rel(0.85)),
      axis.title.x = element_text(size = rel(0.85)),
      axis.title.y = element_text(size = rel(0.85), angle = 90),
      strip.text = element_text(size = 9, face = "bold"),
      strip.background = element_rect(colour = "#ffffff",
                                      fill = "#ffffff"),
      legend.title = element_text(face = "bold", size = 9),
      legend.text = element_text(size = 9),
      legend.position="bottom",
      legend.justification = "center",
      legend.background = element_rect(fill = "transparent",
                                       colour = NA),
      legend.key = element_rect(fill="transparent", colour=NA)
    )
}
