#featurecorrelationmodule
create_heatmap <- function(df, input_var, cluster_cols, cluster_rows, palatte){
    heatmaply(
        df, 
        main = getNiceName(input_var), 
        Colv = cluster_cols, 
        Rowv = cluster_rows,
        colors = palatte,
        margins = c(150, 200, NA, 0)) %>% 
    layout(
        xaxis = list(tickangle = 45),
        font = list(family = "Roboto, Open Sans, sans-serif"))
}