immunomodulator_UI <- function(id) {
    
    ns <- NS(id)
    
    tagList(
        fluidPage(
            # Application title
            titlePanel("Immunomodulators"),
            sidebarLayout(
                sidebarPanel(
                    # Drop-down selected sample groups
                    selectInput(
                        inputId = ns("ss_choice"),
                        label = "Select Sample Groups",
                        choices = as.character(
                            panimmune_data$sample_selection_choices),
                        selected = "Immune Subtype"),
                    
                    # Drop-down selected immuno modulator
                    selectInput(
                        inputId = ns("im_choice"),
                        label = "Select Cellular Content",
                        choices = as.character(
                            panimmune_data$cell_content_choices),
                        selected = "Leukocyte Fraction")
                ),
                
                mainPanel(
                    # Show a plot of the generated distribution
                    plotOutput(ns("distPlot"))
                )
            )    
        )
    )
}

immunomodulator <- function(input, output, session){
    
    output$distPlot <- renderPlot({
        ss_group <- get_internal_name(input$ss_choice)
        im_group <- get_internal_name(input$im_choice)
        plot_df <- create_cellcontent_df(ss_group, im_group)
        plot_colors <- decide_plot_colors(panimmune_data, ss_group)
        plot <- create_boxplot(
            plot_df, 
            x = ss_group, 
            y = im_group, 
            fill_factor = ss_group, 
            x_label = input$ss_choice, 
            y_label = input$im_choice,
            fill_colors = plot_colors)
        print(plot)
    })
}