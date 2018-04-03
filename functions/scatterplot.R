create_scatterplot <- function(
    df, x_column, y_column, label_column = "label",
    x_lab = "", y_lab = "", title = "", corrplot = FALSE
) {
    if (corrplot) {
        axis_max <- max(
            max(df[[x_column]], na.rm = TRUE), 
            max(df[[x_column]], na.rm = TRUE)
        )
        axis_df <- tibble(x = 1:axis_max, y = 1:axis_max)
    }
    
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
    if (corrplot) {
        p %>% 
            layout(
                shapes = list(
                    type = "line", 
                    x0 = 0, 
                    y0 = 0, 
                    x1 = 1, 
                    y1 = 1, 
                    xref = "paper",
                    line = list(color = "red")
                ),
                xaxis = list(range(0, 1)),
                yaxis = list(range(0, 1))
            ) %>% 
            format_plotly() %>%
            I
    } else {
        p %>% 
            format_plotly()
    }
}