create_violinplot <- function(
  df, x, y, fill_factor, xlab, ylab, source_name = NULL, title, fill_colors = NA, 
  facet = NA
) {
  let(
    alias = c(xvar = x, yvar = y, splitvar = fill_factor),
    df %>% 
      plot_ly(
        x = ~xvar,
        y = ~yvar,
        split = ~splitvar,
        color = ~splitvar,
        source = source_name,
        colors = fill_colors,
        type = 'violin',
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
  )
}