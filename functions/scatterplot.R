create_scatterplot <- function(df, x, y, xlab = "", ylab = "", title = "") {
    let(
        alias = c(xvar = x, yvar = y),
        df %>%
            plot_ly(
                x = ~xvar,
                y = ~yvar
            ) %>% 
            layout(
                title = title,
                xaxis = list(title = xlab), 
                yaxis = list(title = ylab)
            )
    )
}