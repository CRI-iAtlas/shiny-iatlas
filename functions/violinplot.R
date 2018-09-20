create_violinplot <- function(
  df, xlab, ylab, source_name = NULL, fill_colors = NA, facet = NA, points = NULL) {
    
    df %>% 
      plot_ly(
        x = ~x,
        y = ~y,
        split = ~x,
        color = ~x,
        points = points,
        source = source_name,
        colors = fill_colors,
        type = 'violin',
        key = ~label,
        box = list(
          visible = TRUE
        ),
        meanline = list(
          visible = TRUE
        )
      ) %>% 
      layout(
        xaxis = list(title = xlab),
        yaxis = list(title = ylab)
      ) %>% 
      format_plotly() %>%
      I
}