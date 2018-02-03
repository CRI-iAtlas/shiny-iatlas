# renderPlotly() also understands ggplot2 objects!
output$corrPlot <- renderPlotly({
    # first build the correlation matrix
    df <- buildDataFrame_corr(corrheatmap_data$dat, input$var1, input$var2, input$catx)
    # then get the heatmap options
    cluster_cols <- as.logical(input$clustercols)
    cluster_rows <- as.logical(input$clusterrows)
    # color scheme
    rwb <- colorRampPalette(colors = c("blue", "white", "red"))
    heatmaply(df, 
              main = getNiceName(input$var2), 
              Colv=cluster_cols, Rowv=cluster_rows,
              colors = rwb,
              margins = c(150,200,NA,0)) %>% 
        layout(xaxis = list(tickangle = 45),
               font = list(family = "Roboto, Open Sans, sans-serif"))
})

output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else d
})