create_boxplot <- function(df, x, y, fill, x_label, y_label){
    df %>% 
        ggplot(aes_string(x, y, fill = fill)) + 
        geom_boxplot() +
        guides(colour = FALSE, fill = FALSE) +
        ylab(y_label) + 
        xlab(x_label) +
        theme_bw() +
        theme_1012 +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
}