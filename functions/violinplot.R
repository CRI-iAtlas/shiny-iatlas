create_violinplot <- function(
    df, 
    xcol = "x",
    ycol = "y",
    key_col = "label",
    split_col = "x",
    color_col = "x",
    xlab = "",
    ylab = "", 
    title = "", 
    source_name = NULL, 
    fill_colors = NA, 
    points = NULL,
    showlegend = T) {
    
    print(df)
    p <- let(
        alias = c(
            X = xcol,
            Y = ycol,
            KEY = key_col,
            SPLIT = split_col,
            COLOR = color_col),
        plot_ly(
            df,
            x = ~X,
            y = ~Y,
            split = ~SPLIT,
            color = ~COLOR,
            key = ~KEY,
            points = points,
            source = source_name,
            colors = fill_colors,
            type = 'violin',
            showlegend = showlegend,
            box = list(
                visible = TRUE
            ),
            meanline = list(
                visible = TRUE
            )
        )) %>% 
        layout(
            title = title,
            xaxis = list(title = xlab),
            yaxis = list(title = ylab)
        ) %>% 
        format_plotly() %>%
        I
}