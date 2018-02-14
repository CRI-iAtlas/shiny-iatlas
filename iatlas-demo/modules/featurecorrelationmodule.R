featurecorrelation_UI <- function(id) {
    
    ns <- NS(id)
    
    tagList(
        fluidPage(
            titlePanel("Immune Feature Correlation Heatmap"),
            sidebarLayout(
                sidebarPanel(
                    
                    selectInput(
                        ns("var1"), 
                        "Variable 1", 
                        c("Leukocytes", 
                          "Gene Sets",
                          "Genes RNA-seq & RPPA"), 
                        selected = "Leukocytes"),
                    
                    selectInput(
                        ns("var2"), 
                        "Variable 2", 
                        c("Leukocyte Fraction"="leukocyte_fraction",
                          "OS Time"="OS_time",
                          "Mutation Rate, Non-Silent"="mutationrate_nonsilent_per_Mb",
                          "Indel Neoantigens"="indel_neoantigen_num",
                          "SNV Neoantigens"="numberOfImmunogenicMutation",
                          "Stemness Score RNA"="StemnessScoreRNA"),
                        selected = "Leukocyte Fraction"),
                    
                    selectInput(
                        ns("catx"), 
                        "Category",   
                        c("Immune Subtypes"="Subtype_Immune_Model_Based",
                          "TCGA Tissues"="Study",
                          "TCGA Subtypes"="Subtype_Curated_Malta_Noushmehr_et_al"), 
                        selected = "Immune Subtypes"),
                    
                    checkboxInput(ns("clustercols"), label = "Cluster Columns", value = F),
                    checkboxInput(ns("clusterrows"), label = "Cluster Rows", value = T)
                ),
                
                mainPanel(
                    plotlyOutput(ns("corrPlot"), height = "600px"),
                    HTML("<br><br><br>")
                    #verbatimTextOutput("event")
                )
            )
        )
    )
}

featurecorrelation <- function(input, output, session){
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
}