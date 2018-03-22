create_boxplot <- function(df, x, y, fill_factor, x_label, y_label, fill_colors = NA, facet = NA, title = NULL) {
  plot <- df %>%
    ggplot(aes_string(x, y, fill = fill_factor)) +
    geom_boxplot() +
    guides(colour = FALSE, fill = FALSE) +
    ylab(y_label) +
    xlab(x_label) +
    theme_bw() +
    theme_1012 +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    labs(title = title)
  if (!is.na(fill_colors)) {
    plot <- plot + scale_fill_manual(values = fill_colors)
  }
  if (!is.na(facet)) {
    plot <- plot + facet_grid(facet)
  }
  return(plot)
}
