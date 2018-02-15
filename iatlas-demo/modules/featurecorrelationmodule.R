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
                    
                    checkboxInput(
                        ns("clustercols"), 
                        label = "Cluster Columns", 
                        value = F),
                    
                    checkboxInput(
                        ns("clusterrows"), 
                        label = "Cluster Rows", 
                        value = T)
                ),
                
                mainPanel(
                    plotlyOutput(ns("corrPlot"), height = "600px"),
                    HTML("<br><br><br>")
                )
            )
        )
    )
}

featurecorrelation <- function(input, output, session){
    output$corrPlot <- renderPlotly({
        # first build the correlation matrix
        df <- buildDataFrame_corr(panimmune_data$df, input$var1, input$var2, input$catx)
        # then get the heatmap options
        cluster_cols <- as.logical(input$clustercols)
        cluster_rows <- as.logical(input$clusterrows)
        # color scheme
        colors <- colorRampPalette(colors = c("blue", "white", "red"))
        input_var <- getNiceName(input$var2)
        create_heatmap(df, input_var, cluster_cols, cluster_rows, colors)
    })
}