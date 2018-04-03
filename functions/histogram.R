create_histogram <- function(
  df, x_column, x_lab, y_lab = "Count", title = NULL
) {
  let(
    alias = c(xvar = x_column),
    df %>% 
      plot_ly(
        x = ~xvar
      ) %>% 
      add_histogram(alpha = 0.8) %>% 
      layout(
        title = title,
        xaxis = list(title = x_lab),
        yaxis = list(title = y_lab)
      ) %>% 
      format_plotly() %>% 
      I
  )
}
