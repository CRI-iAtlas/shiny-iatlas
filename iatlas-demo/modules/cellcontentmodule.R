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
                                choices=as.character(cellcontent_data$sampleselectionchoices),
                                selected="Immune Subtype"),
                    
                    # Drop-down selected cell content
                    selectInput(inputId=ns("cellcontentchoice"),
                                label="Select Cellular Content",
                                choices=as.character(cellcontent_data$cellcontentchoices),
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
        sample_group_label <- get_label_from_data_obj(cellcontent_data, "samplegroups", input$selectionchoice)
        cellcontent_label <- get_label_from_data_obj(cellcontent_data, "cellcontent", input$cellcontentchoice)
        ## create dfp, the data frame for plotting, based on choices
        plot_df <- create_cellcontent_df(sample_group_label, cellcontent_label)
        ## custom colors if available 
        plot_colors <- decide_plot_colors(cellcontent_data, sample_group_label)
        plot <- create_boxplot(
            plot_df, 
            x = sample_group_label, 
            y = cellcontent_label, 
            fill_factor = sample_group_label, 
            x_label = input$selectionchoice, 
            y_label = input$cellcontentchoice,
            fill_colors = plot_colors)
        print(plot)
    })
}