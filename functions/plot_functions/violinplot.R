create_violinplot <- function(
    df, 
    x_col = "x",
    y_col = "y",
    key_col = NA,
    color_col = NA,
    label_col = NA,
    split_col = NA,
    xlab = "",
    ylab = "", 
    title = "", 
    source_name = NULL, 
    fill_colors = NA, 
    points = NULL,
    showlegend = T) {
    
    
    if(is.na(key_col)) key_col <- x_col
    if(is.na(color_col)) color_col <- x_col
    if(is.na(label_col)) label_col <- x_col
    if(is.na(split_col)) split_col <- x_col
    
    wrapr::let(
        alias = c(
            X = x_col,
            Y = y_col,
            KEY = key_col,
            COLOR = color_col,
            SPLIT = split_col,
            LABEL = label_col),
        plotly::plot_ly(
            df,
            x = ~X,
            y = ~Y,
            split = ~SPLIT,
            color = ~COLOR,
            key = ~KEY,
            text = ~LABEL,
            points = points,
            source = source_name,
            colors = fill_colors,
            type = 'violin',
            hoverinfo = 'text',
            showlegend = showlegend,
            box = list(
                visible = TRUE
            ),
            meanline = list(
                visible = TRUE
            ))) %>%
        plotly::layout(
            title = title,
            xaxis = list(title = xlab),
            yaxis = list(title = ylab)
        ) %>% 
        format_plotly() %>%
        I
}