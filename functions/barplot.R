create_barplot <- function(
    df, 
    x_column, 
    y_column, 
    color_column, 
    error_column, 
    label_column = "label",
    x_lab, 
    y_lab, 
    source_name, 
    bar_colors = NULL ) {
    
    if (is.null(bar_colors)) {
        bar_colors <- viridis::viridis_pal(option = "D")(n_distinct(df[[color_column]]))
    }
    let(
        alias = c(xvar = x_column, 
                  yvar = y_column, 
                  colorvar = color_column,
                  errorvar = error_column,
                  labelvar = label_column),
        df %>% 
            plot_ly(
                x = ~xvar,
                y = ~yvar,
                color = ~colorvar,
                type = 'bar',
                source = source_name,
                colors = bar_colors,
                error_y = list(array = ~errorvar, 
                               color = 'black', 
                               thickness = 1),
                hoverinfo = 'text',
                text = ~labelvar
            ) %>% 
            layout(
                legend = list(orientation = 'h', x = 0, y = 1),
                xaxis = list(title = x_lab),
                yaxis = list(title = y_lab)
            ) %>% 
            format_plotly()
    )
}

