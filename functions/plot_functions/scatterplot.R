create_scatterplot <- function(
    df, 
    x_col = "x",
    y_col = "y",
    key_col = NA,
    color_col = NA, 
    label_col = NA,
    xlab = "", 
    ylab = "", 
    title = "", 
    identity_line = FALSE,
    source_name = NULL,
    fill_colors = NA,
    horizontal_line = FALSE,
    horizontal_line_y = NULL) {
    
    if(is.na(key_col)) key_col <- x_col
    if(is.na(color_col)) color_col <- x_col
    if(is.na(label_col)) label_col <- x_col
    
    p <- 
        wrapr::let(
            alias = c(
                X = x_col,
                Y = y_col,
                KEY = key_col,
                COLOR = color_col,
                LABEL = label_col), 
            plotly::plot_ly(
                df,
                x = ~X,
                y = ~Y,
                color = ~COLOR,
                colors = fill_colors,
                text = ~LABEL,
                key = ~KEY,
                source = source_name) %>% 
                plotly::add_markers(
                    alpha = 0.5,
                    hoverinfo = 'text',
                    text = ~LABEL,
                    textposition = 'top left'
                ))
            
    p <- p %>% 
        plotly::layout(
            title = title,
            xaxis = list(title = xlab), 
            yaxis = list(title = ylab)
        )
    
    if (horizontal_line) {
        p <- p %>%
            plotly::layout(
                shapes = list(
                    type = "line",
                    x0 = -1,
                    y0 = horizontal_line_y,
                    x1 = 1,
                    y1 = horizontal_line_y,
                    line = list(color = "black", dash = "dot", alpha = 0.5)
                ))
    }
    
    if (identity_line) {
        p %>% 
            plotly::layout(
                shapes = list(
                    type = "line", 
                    x0 = 0, 
                    y0 = 0, 
                    x1 = 1, 
                    y1 = 1, 
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