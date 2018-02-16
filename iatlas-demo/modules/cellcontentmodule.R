cellcontent_UI <- function(id) {
    
    ns <- NS(id)
    
    tagList(
        fluidPage(
            # Application title
            titlePanel("Immune Cell Content by Sample Group"),
            sidebarLayout(
                sidebarPanel(
                    # Drop-down selected sample groups
                    selectInput(inputId=ns("sample_selection_choice"),
                                label="Select Sample Groups",
                                choices=as.character(panimmune_data$sample_selection_choices),
                                selected="Immune Subtype"),
                    
                    # Drop-down selected cell content
                    selectInput(inputId=ns("cell_content_choice"),
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
        sample_selection_label <- get_internal_name(input$sample_selection_choice)
        cell_content_label <- get_internal_name(input$cell_content_choice)
        plot_df <- create_cellcontent_df(sample_selection_label, cell_content_label)
        plot_colors <- decide_plot_colors(panimmune_data, sample_selection_label)
        plot <- create_boxplot(
            plot_df, 
            x = sample_selection_label, 
            y = cell_content_label, 
            fill_factor = sample_selection_label, 
            x_label = input$sample_selection_choice, 
            y_label = input$cell_content_choice,
            fill_colors = plot_colors)
        print(plot)
    })
}