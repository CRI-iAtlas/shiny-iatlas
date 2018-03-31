create_plotly_heatmap <- function(matrix, source_name){
    plot_ly(
        z = matrix,
        x = colnames(matrix),
        y = rownames(matrix),
        type = "heatmap",
        source = source_name,
        colors = rev(RColorBrewer::brewer.pal(8, "RdBu"))) %>% 
    format_plotly()
}