create_violinplot <- function(
  df, xlab, ylab, 
  title = "", 
  source_name = NULL, 
  fill_colors = NA, 
  facet = NA, 
  showlegend = TRUE
  ) {

    p <- df %>% 
      plot_ly(
        x = ~x,
        y = ~y,
        split = ~x,
        color = ~x,
        source = source_name,
        colors = fill_colors,
        type = 'violin',
        box = list(
          visible = TRUE
        ),
        meanline = list(
          visible = TRUE
        ),
        showlegend = showlegend
      ) %>% 
      layout(
        title = title,
        xaxis = list(title = xlab),
        yaxis = list(title = ylab)
      ) 
    

    p %>% 
      format_plotly() %>%
      I
}