create_scatterplot <- function(
  df, x_column, y_column, x_lab = "", y_lab = "", title = "", corrplot = FALSE
) {
  if (corrplot) {
    axis_max <- max(
      max(df[[x_column]], na.rm = TRUE), 
      max(df[[x_column]], na.rm = TRUE)
    )
    axis_df <- tibble(x = 1:axis_max, y = 1:axis_max)
  }
  
  let(
    alias = c(xvar = x_column, yvar = y_column),
    p <- df %>%
      plotly::plot_ly(
        x = ~xvar,
        y = ~yvar
      ) %>% 
      add_markers(
        alpha = 0.5
      )
    )
  if (corrplot) {
    p <- p %>% 
      layout(
        xaxis = list(range(0, 1)),
        yaxis = list(range(0, 1))
      )
  }
  p %>% 
    layout(
      title = title,
      xaxis = list(title = x_lab), 
      yaxis = list(title = y_lab)
    ) %>% 
    format_plotly()
}