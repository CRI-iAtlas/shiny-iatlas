create_mosaicplot <- function(
    df, x, y, fill_factor, 
    xlab = NULL, 
    ylab = NULL, 
    title = NULL, 
    fill_colors = NA, 
    facet = NA,
    width = 1500){
    
    
    plot <- df %>%
        ggplot(aes_string(x = str_c("product(", y, ",", x, ")")  )) +
        geom_mosaic(aes_string(fill = y)) +
        theme_bw() +
        theme_1012 +
        xlab(xlab) +
        ylab(ylab) +
        labs(title) +
        theme(legend.title = element_blank()) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    if (!is.na(fill_colors)) {
        plot <- plot + scale_fill_manual(values = fill_colors)
    }
    if (!is.na(facet)) {
        plot <- plot + facet_grid(facet)
    }
    ggplotly(plot, height = 600, width = width)
}

