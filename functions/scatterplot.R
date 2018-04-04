create_scatterplot <- function(
  df, x_column, y_column, label_column = "label",
  x_lab = "", y_lab = "", title = "", identity_line = FALSE
) {
  let(
    alias = c(xvar = x_column, yvar = y_column, labelvar = label_column),
    p <- df %>%
      plotly::plot_ly(
        x = ~xvar,
        y = ~yvar
      ) %>% 
      add_markers(
        alpha = 0.5,
        hoverinfo = 'text',
        text = ~labelvar,
        textposition = 'top left'
      )
  )
  p <- p %>% 
    layout(
      title = title,
      xaxis = list(title = x_lab), 
      yaxis = list(title = y_lab)
    )
  if (identity_line) {
    p %>% 
      layout(
        shapes = list(
          type = "line", 
          x0 = 0, 
          y0 = 0, 
          x1 = 1, 
          y1 = 1, 
          # xref = "paper",
          line = list(color = "black", dash = "dot", alpha = 0.5)
        ),
        xaxis = list(range(0, 1)),
        yaxis = list(range(0, 1))
      ) %>% 
      format_plotly()
  } else {
    p %>% 
      format_plotly()
  }
}