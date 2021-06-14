options(ggplot2.continuous.colour = "viridis")
options(ggplot2.discrete.colour = "viridis")

colors <- viridis(n = 5, option = "plasma")
names(colors) <- c("Metric", "Baseline", "Disturbed", "LRR", "Difference")
resp_colscale <- scale_colour_manual(values = colors)

## possibly color-clind safe color scheme
intensity_colors = c("#67001f", "#a50026", "#d73027", "#f46d43", "#fdae61",
                     "#fee090", "#ffffbf", "#e0f3f8", "#abd9e9", "#74add1",
                     "#4575b4", "#313695")

## base theme for plotting figures with graphs for all disturbances and figures: main issue is font size, but watch for facetting
theme_estar <- function(){
    theme_minimal(base_size = 8)%+replace%
        theme(
            axis.text = element_text(size = rel(1.25)),
            axis.title.x = element_text(size = rel(1.25)),
            axis.title.y = element_text(size = rel(1.25), angle = 90),
            strip.text = element_text(size = 10, face = "bold"),
            strip.background = element_rect(colour = "#E6E6F4",
                                            fill = "#E6E6F4"),
            legend.title = element_text(face = "bold", size = 10),
            legend.text = element_text(size = 10),
            legend.position="bottom",
            legend.background = element_rect(fill = "transparent",
                                             colour = NA),
            legend.key = element_rect(fill="transparent", colour=NA)
    )
}
