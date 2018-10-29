create_mosaicplot <- function(
    df, 
    title = NULL, 
    fill_colors = NA,
    xlab = NULL, 
    ylab = NULL) {

    plot <- df %>%
        ggplot() +
        ggmosaic::geom_mosaic(aes_string(x = str_c("product(y, x)") ,fill = "y")) +
        scale_y_productlist(expand = c(0, 0)) + 
        scale_x_productlist(expand = c(0, 0)) + 
        labs(title) +
        xlab(xlab) +
        ylab(ylab) +
        theme_minimal() +
        theme(
            legend.title = element_blank(),
            axis.text.x = element_text(angle = 90, hjust = 1),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
        )
    if (!is.na(fill_colors[[1]])) {
        plot <- plot + scale_fill_manual(values = fill_colors)
    }
    p <- plotly_build(plot) %>%
        layout(title = title,
               legend = list(traceorder = 'reversed')) %>% 
        format_plotly()
    p["width"] <- list(NULL)
    p["height"] <- list(NULL)
    p$x$layout["width"] <- list(NULL)
    p$x$layout["height"] <- list(NULL)
    p
}

