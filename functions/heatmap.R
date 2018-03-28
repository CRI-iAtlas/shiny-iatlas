create_plotly_heatmap <- function(matrix){
    plot_ly(
        z = matrix,
        x = colnames(matrix),
        y = rownames(matrix),
        type = "heatmap",
        source = "heatplot",
        colors = colorRamp(c("blue", "white", "red")))
}