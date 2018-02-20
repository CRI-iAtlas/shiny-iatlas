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
                        label = "Select Immunomodulator Gene",
                        choices = as.character(panimmune_data$direct_relationship_modulators$HGNC_Symbol))
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
        im_group <- input$im_choice
        plot_df <- panimmune_data$immunomodulator_df %>% 
            filter(Symbol == im_group) %>% 
            left_join(panimmune_data$df) %>% 
            mutate(log_count = log10(normalized_count + 1)) %>% 
            select(ss_group, log_count) %>% 
            .[complete.cases(.), ] 
        plot_colors <- decide_plot_colors(panimmune_data, ss_group)
        plot <- create_boxplot(
            plot_df, 
            x = ss_group, 
            y = "log_count", 
            fill_factor = ss_group, 
            x_label = input$ss_choice, 
            y_label = "Log10 (Count + 1)",
            fill_colors = plot_colors)
        print(plot)
    })
}