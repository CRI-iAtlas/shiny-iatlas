create_mosaicplot <- function(
  df, x, y, fill_factor, 
  xlab = NULL, 
  ylab = NULL, 
  title = NULL, 
  fill_colors = NA, 
  facet = NA,
  width = 1500) {
  
  
  plot <- df %>%
    ggplot(aes_string(x = str_c("product(", y, ",", x, ")")  )) +
    ggmosaic::geom_mosaic(aes_string(fill = y)) +
    scale_y_productlist(expand = c(0, 0)) + 
    scale_x_productlist(expand = c(0, 0)) + 
    xlab(xlab) +
    ylab(ylab) +
    labs(title) +
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 90, hjust = 1),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  if (!is.na(fill_colors)) {
    plot <- plot + scale_fill_manual(values = fill_colors)
  }
  if (!is.na(facet)) {
    plot <- plot + facet_grid(facet)
  }
  ggplotly(plot, height = 600, width = width) %>% 
    layout(title = title,
           legend = list(traceorder = 'reversed')) %>% 
    format_plotly() %>%
    I
}

