create_plotly_scatterplot <- function(plot){
    plot %>% 
        ggplotly()
        # layout(margin = list(
        #     l = 100,
        #     r = 40,
        #     b = 50,
        #     t = 100,
        #     pad = 2
        # ))
}


create_gg_scatterplot <- function(df, x, y, xlab = "", ylab = "", title = ""){
    df %>%
        ggplot(aes_string(x, y)) +
        geom_point() +
        theme_bw() +
        theme_1012 +
        xlab(xlab) +
        ylab(ylab) +
        labs(title)
}