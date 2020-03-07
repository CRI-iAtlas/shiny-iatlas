create_histogram <- function(
    df, 
    x_col = "x", 
    x_lab = "", 
    y_lab = "Count", 
    title = "",
    source_name = NULL
) {
    wrapr::let(
        alias = c(
            X = x_col
        ),
        df %>% 
            plot_ly(
                x      = ~X,
                source = source_name,
                type   = "histogram"
            ) %>% 
            layout(
                title = title,
                xaxis = list(title = x_lab),
                yaxis = list(title = y_lab)
            ) %>% 
            format_plotly() %>% 
            I
    )
}
