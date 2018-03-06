immunomodulator2_UI <- function(id) {
    
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
                    plotlyOutput(ns("boxPlot")),
                    plotlyOutput(ns("histPlot"))
                )
            )    
        )
    )
}

immunomodulator2 <- function(input, output, session){
    
    ss_group <- reactive(get_variable_internal_name(input$ss_choice))
    
    plot_df <- reactive({
        plot_df <- panimmune_data$immunomodulator_df %>% 
            filter(Symbol == input$im_choice) %>% 
            left_join(panimmune_data$df) %>% 
            mutate(log_count = log10(normalized_count + 1)) %>% 
            select(ss_group(), log_count) %>% 
            .[complete.cases(.), ] 
    })
    
    output$boxPlot <- renderPlotly({
        plot_colors <- decide_plot_colors(panimmune_data, ss_group())
        plot <- create_boxplot(
            plot_df(), 
            x = ss_group(), 
            y = "log_count", 
            fill_factor = ss_group(), 
            x_label = input$ss_choice, 
            y_label = "Log10 (Count + 1)",
            fill_colors = plot_colors,
            title = get_modulator_display_name(input$im_choice))
        print(ggplotly(plot, source = "select"))
    })
    
    output$histPlot <- renderPlotly({
        
        eventdata <- event_data("plotly_click", source = "select")
        validate(need(!is.null(eventdata), "Click boxplot"))
        boxplot_n <- eventdata$x[[1]]

        boxplot_selected_group <- plot_df() %>% 
            extract2(ss_group()) %>% 
            as.factor %>% 
            levels %>% 
            extract2(boxplot_n)
        
        dd_plot_df <- plot_df() %>% 
            filter(UQ(as.name(ss_group())) ==  boxplot_selected_group)
        
        plot <- dd_plot_df %>% 
            ggplot(aes_string("log_count")) +
            geom_histogram() + 
            theme_bw() +
            theme_1012 +
            ylab("Count") + 
            xlab("Log count") +
            ggtitle(boxplot_selected_group) +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
        print(ggplotly(plot))
    })
}