create_plotly_heatmap <- function(matrix){
    plot_ly(
        z = matrix,
        x = colnames(matrix),
        y = rownames(matrix),
        type = "heatmap",
        source = "heatplot",
        colors = colorRamp(c("blue", "white", "red"))) %>%
        layout(margin = list(
            l = 250,
            r = 10,
            b = 100,
            t = 10,
            pad = 2
        ))
}