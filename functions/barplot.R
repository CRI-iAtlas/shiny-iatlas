## Create a bar plot with standard error bars using plotly

create_barplot <- function(
    df, x, y, color_var, sd_var, xlab, ylab, bar_colors
) {
    let(
        alias = c(xvar = x, 
                  yvar = y, 
                  colorvar = color_var,
                  sdvar = sd_var),
        df %>% 
            plot_ly(
                x = ~xvar,
                y = ~yvar,
                color = ~colorvar,
                type = 'bar',
                colors = bar_colors,
                error_y = list(value = ~sdvar, color = 'black')) %>% 
            layout(
                xaxis = list(title = xlab),
                yaxis = list(title = ylab)
            )
    )
}

