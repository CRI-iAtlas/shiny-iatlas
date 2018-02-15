cellcontent_UI <- function(id) {
    
    ns <- NS(id)
    
    tagList(
        fluidPage(
            # Application title
            titlePanel("Immune Cell Content by Sample Group"),
            sidebarLayout(
                sidebarPanel(
                    # Drop-down selected sample groups
                    selectInput(inputId=ns("selectionchoice"),
                                label="Select Sample Groups",
                                choices=as.character(panimmune_data$sample_selection_choices),
                                selected="Immune Subtype"),
                    
                    # Drop-down selected cell content
                    selectInput(inputId=ns("cellcontentchoice"),
                                label="Select Cellular Content",
                                choices=as.character(panimmune_data$cell_content_choices),
                                selected="Leukocyte Fraction")
                ),
                
                mainPanel(
                    # Show a plot of the generated distribution
                    plotOutput(ns("distPlot"))
                )
            )    
        )
    )
}

cellcontent <- function(input, output, session){
    
    output$distPlot <- renderPlot({
        sampgroup <- as.character(panimmune_data$sample_groups[input$selectionchoice]) ## the label at the data source
        cellcontent <- as.character(panimmune_data$cell_content[input$cellcontentchoice])
        ## create dfp, the data frame for plotting, based on choices
        if ( USE_REMOTE) { 
            bq <- paste('SELECT ',sampgroup," , ", cellcontent," FROM [isb-cgc-01-0007:Feature_Matrix.PanImmune_FMx]",
                        " where ",cellcontent," is not null and ",sampgroup," is not null")
            dfp <- query_exec(bq,project="isb-cgc-01-0007") }
        else {
            dfp <- panimmune_data$df %>% select(sampgroup,cellcontent) %>% .[complete.cases(.),]
        }
        
        ## custom colors if available 
        if (sampgroup=='Study'){plotcolors <- panimmune_data$tcga_colors}
        else if (sampgroup=='Subtype_Immune_Model_Based') {plotcolors <- panimmune_data$subtype_colors}
        
        print(input$selectionchoice)
        print(input$cellcontentchoice)
        
        p <- create_boxplot(dfp, x = sampgroup, y = cellcontent, fill = sampgroup, input$selectionchoice, input$cellcontentchoice)
        if ( sampgroup %in% c('Study','Subtype_Immune_Model_Based'))
        {p <- p + scale_fill_manual(values=plotcolors )}
        print(p)
    })
}

