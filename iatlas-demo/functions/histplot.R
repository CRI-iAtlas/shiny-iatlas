create_histplot <- function(df, x, x_label, y_label = "Count", title = NULL) {
  plot <- df %>%
    ggplot(aes_string(x)) +
    geom_histogram() +
    ylab(y_label) +
    xlab(x_label) +
    theme_bw() +
    theme_1012 +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    labs(title = title)
  return(plot)
}
