options(ggplot2.continuous.colour = "viridis")
options(ggplot2.discrete.colour = "viridis")
options(ggplot2.fill.colour = "viridis")
options(ggplot2.fill.colour = "viridis")

colors <- viridis(n = 5, option = "inferno")
#"#000004FF" "#56106EFF" "#BB3754FF" "#F98C0AFF" "#FCFFA4FF"

## possibly color-clind safe color scheme
#c("#67001f", "#a50026", "#d73027", "#f46d43", "#fdae61",
#  "#fee090", "#ffffbf", "#e0f3f8", "#abd9e9", "#74add1",
#  "#4575b4", "#313695")

gp_colours <- c("#313695", "#DD3F4A", "#FFCF37")
treat_colours <- c("#B256B4", "#8E2E79")

## base theme for plotting figures with graphs for all disturbances and figures: main issue is font size, but watch for facetting
theme_estar <- function(){
    #theme_minimal(base_size = 8)%+replace%
    theme_minimal_hgrid(12)%+replace%
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

