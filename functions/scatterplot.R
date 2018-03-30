create_scatterplot <- function(
    df, x_column, y_column, x_lab = "", y_lab = "", title = ""
) {
    let(
        alias = c(xvar = x_column, yvar = y_column),
        df %>%
            plot_ly(
                x = ~xvar,
                y = ~yvar
            ) %>% 
            layout(
                title = title,
                xaxis = list(title = x_lab), 
                yaxis = list(title = y_lab)
            )
    )
}