create_scatterplot <- function(
    df, 
    xlab = "", 
    ylab = "", 
    title = "", 
    identity_line = FALSE,
    source = NULL) {
    
    p <- df %>%
        plotly::plot_ly(
            x = ~x,
            y = ~y,
            source = source,
            key = ~label
        ) %>% 
        add_markers(
            alpha = 0.5,
            hoverinfo = 'text',
            text = ~label,
            textposition = 'top left'
        )
    
    p <- p %>% 
        layout(
            title = title,
            xaxis = list(title = xlab), 
            yaxis = list(title = ylab)
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