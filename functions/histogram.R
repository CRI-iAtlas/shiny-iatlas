create_histogram <- function(
  df, x_column, x_lab, y_lab = "Count", title = NULL
) {
  let(
    alias = c(xvar = x_column),
    df %>% 
      plot_ly(
        x = ~xvar
      ) %>% 
      add_histogram() %>% 
      layout(
        title = title,
        xaxis = list(title = x_lab) 
      )
  )
  # plot <- df %>%
  #   ggplot(aes_string(x)) +
  #   geom_histogram() +
  #   ylab(y_label) +
  #   xlab(x_label) +
  #   theme_bw() +
  #   theme_1012 +
  #   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  #   labs(title = title)
  # return(plot)
}
