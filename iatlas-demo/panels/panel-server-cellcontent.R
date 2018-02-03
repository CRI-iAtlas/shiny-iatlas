output$distPlot <- renderPlot({
    
    sampgroup <- as.character(cellcontent_data$samplegroups[input$selectionchoice]) ## the label at the data source
    cellcontent <- as.character(cellcontent_data$cellcontent[input$cellcontentchoice])
    
    ## create dfp, the data frame for plotting, based on choices
    if ( USE_REMOTE) { 
        bq <- paste('SELECT ',sampgroup," , ", cellcontent," FROM [isb-cgc-01-0007:Feature_Matrix.PanImmune_FMx]",
                    " where ",cellcontent," is not null and ",sampgroup," is not null")
        dfp <- query_exec(bq,project="isb-cgc-01-0007") }
    else {
        dfp <- cellcontent_data$df %>% select(sampgroup,cellcontent) %>% .[complete.cases(.),]
    }
    
    ## custom colors if available 
    if (sampgroup=='Study'){plotcolors <- cellcontent_data$tcga_colors}
    else if (sampgroup=='Subtype_Immune_Model_Based') {plotcolors <- cellcontent_data$subtype_colors}
    
    p <- ggplot(dfp,aes_string(sampgroup,cellcontent,fill=sampgroup)) + geom_boxplot() +
        guides(colour = FALSE, fill = FALSE) +
        ylab(input$cellcontentchoice) + xlab(input$selectionchoice) +
        theme_bw() +
        theme_1012  + 
        theme(axis.text.x = element_text(angle=90,vjust = 0.5)) 
    if ( sampgroup %in% c('Study','Subtype_Immune_Model_Based'))
    {p <- p + scale_fill_manual(values=plotcolors )}
    print(p)
})