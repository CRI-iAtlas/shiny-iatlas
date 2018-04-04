create_heatmap <- function(corr_mat, source_name){
  plot_ly(
    z = corr_mat,
    x = colnames(corr_mat),
    y = rownames(corr_mat),
    type = "heatmap",
    source = source_name,
    colors = rev(RColorBrewer::brewer.pal(8, "RdBu"))) %>% 
    layout(xaxis = list(tickangle = 90)) %>% 
    format_plotly() %>%
    I
}