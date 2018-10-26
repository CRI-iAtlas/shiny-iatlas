create_histogram <- function(
    df, 
    x_col = "x", 
    key_col = NA,
    label_col = NULL,
    x_lab = "", 
    y_lab = "Count", 
    title = "",
    source_name = NULL) {
    
    if(is.na(key_col)) key_col <- x_col
    
    if(is.null(label_col)){
        #do nothing
    } else if(is.na(label_col)){
        label_col <- x_col
    }
    
    let(
        alias = c(
            X = x_col, 
            KEY = key_col,
            LABEL = label_col
        ),
        df %>% 
            plot_ly(
                x = ~X,
                key = ~KEY,
                text = label_col
            ) %>% 
            add_histogram(alpha = 0.8) %>% 
            layout(
                title = title,
                xaxis = list(title = x_lab),
                yaxis = list(title = y_lab)
            ) %>% 
            format_plotly() %>% 
            I
    )
}
