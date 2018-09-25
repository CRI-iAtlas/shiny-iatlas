create_boxplot <- function(
    df, 
    xlab = "",
    ylab = "", 
    source_name = NULL, 
    fill_colors = NA){
    
    p <- df %>% 
        plot_ly(
            x = ~x,
            y = ~y,
            split = ~x,
            color = ~x,
            type = "box", 
            boxpoints = "all", 
            jitter = 0.7,
            pointpos = 0, 
            colors = fill_colors,
            source = source_name,
            key = ~label) %>% 
        layout(
            xaxis = list(title = xlab),
            yaxis = list(title = ylab)
        ) %>% 
        format_plotly() %>%
        I
        
    
}
