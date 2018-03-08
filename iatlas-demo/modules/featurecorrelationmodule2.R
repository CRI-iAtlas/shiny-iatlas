featurecorrelation_UI2 <- function(id) {
    
    ns <- NS(id)
    
    tagList(
        fluidPage(
            titlePanel("Immune Feature Correlation Heatmap"),
            sidebarLayout(
                sidebarPanel(
                    
                    selectInput(
                        ns("var1"), 
                        "Variable 1", 
                        c("Core Expression Signature",
                          "DNA Alteration",
                          "Adaptive Receptor",
                          "T Helper Cell Score",
                          "Immune Cell Proportion - Original",
                          "Immune Cell Proportion - Aggregate 1",
                          "Immune Cell Proportion - Aggregate 2",
                          "Immune Cell Proportion - Aggregate 3"),
                        selected = "Immune Cell Proportion - Aggregate 2"),
                    
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
                    plotlyOutput(ns("scatterPlot")),
                    HTML("<br><br><br>")
                )
            )
        )
    )
}

featurecorrelation2 <- function(input, output, session){
    
    
    output$corrPlot <- renderPlotly({
        # first build the correlation matrix
        df <- buildDataFrame_corr(panimmune_data$df, "Immune Cell Proportion - Aggregate 2", "leukocyte_fraction", "Subtype_Immune_Model_Based") 
        df <- buildDataFrame_corr(panimmune_data$df, input$var1, input$var2, input$catx) 
        # then get the heatmap options
        # cluster_cols <- as.logical(input$clustercols)
        # cluster_rows <- as.logical(input$clusterrows)
        # color scheme
        # colors_f <- colorRampPalette(colors = c("blue", "white", "red"))
        input_var <- get_variable_display_name(input$var2)
        # p <- create_heatmap(df, get_variable_display_name("leukocyte_fraction"), T,T, colors)
        # create_heatmap(df, input_var, cluster_cols, cluster_rows, colors)
        plot_ly(z = df,
                x = colnames(df),
                y = rownames(df),
                type = "heatmap",
                source = "heatplot", 
                colors = colorRamp(c("blue", "white", "red")))
    })
    
    output$scatterPlot <- renderPlotly({
        
        eventdata <- event_data("plotly_click", source = "heatplot")
        validate(need(!is.null(eventdata), "Click heatmap"))
        
        select_x <- eventdata$x[[1]]
        df1 <- get_scatterplot_df(panimmune_data$df, input$var1, input$var2, input$catx) %>% 
            filter(UQ(as.name(as.character(input$catx))) == select_x)
        select_y <- get_variable_internal_name(eventdata$y[[1]]) %>% 
            .[. %in% colnames(df1)]
        df2 <- df1 %>%
            select_(.dots = input$var2, select_y) 
        plot <- df2 %>% 
            ggplot(aes_string(x = input$var2, y = select_y)) +
            geom_point () + 
            theme_bw() +
            theme_1012 +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
    })
}


get_scatterplot_df <- function(dat, var1, var2, catx){
    # dat  <- panimmune_data$df
    # var1 <- "Immune Cell Proportion - Aggregate 2"
    # var2 <- "leukocyte_fraction"
    # catx <- "Subtype_Immune_Model_Based"
    
    getCats <- function(dat, catx) {
        cats <- as.character(na.omit(unique(dat[,catx])))
    }
    
    # get the vectors associated with each term
    cats <- sort(getCats(dat, catx))
    vars <- get_variable_group(var1)
    df2 <- dat %>% 
        as_data_frame %>% 
        filter(UQ(as.name(catx)) %in% cats) %>% 
        select_(.dots = c(catx, var2, as.character(vars)))
}