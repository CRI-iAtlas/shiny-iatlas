distributions_plot_module_UI <- function(
    id, 
    title_text = "Distributions",
    message_html = p(stringr::str_c(
        "Select variable to its to see its distribution over sample groups.",
        "Plots are available as violin plots, and box plots with full data",
        "points superimposed."
    )),
    click_text = "Click plot to see group information."
){
    
    ns <- NS(id)
    
    sectionBox(
            title = title_text,
            messageBox(width = 12, message_html),
            fluidRow(
                optionsBox(
                    width = 12,
                    conditionalPanel(
                        condition =  "output.display_group_choice",
                        column(width = 4,uiOutput(ns("group_choice_ui"))),
                        ns = ns
                    ),
                    column(
                        width = 4,
                        uiOutput(ns("variable_choice_ui"))
                    ),
                    column(
                        width = 4,
                        selectInput(
                            ns("plot_type"),
                            "Select Plot Type",
                            choices = c("Violin", "Box")
                        )
                    ),
                    column(
                        width = 4,
                        checkboxInput(ns("log_scale"), "Log Scale Y Axis?", FALSE)
                    )
                )
            ),
            fluidRow(
                plotBox(
                    width = 12,
                    plotlyOutput(ns("plot")) %>%
                        shinycssloaders::withSpinner(),
                    p(),
                    textOutput(ns("plot_text")),
                    h4(click_text)
                )
            ),
            fluidRow(
                plotBox(
                    width = 12,
                    plotlyOutput(ns("histPlot")) %>% 
                        shinycssloaders::withSpinner()
                )
            )
        )
}


distributions_plot_module <- function(
    input, 
    output, 
    session,
    plot_source_name,
    data_df,
    relationship_df,
    sample_group_df,
    plot_colors,
    ...
){
    
    ns <- session$ns
    
    num_group_columns <- reactive({
        req(relationship_df())
        relationship_df() %>% 
            dplyr::select(-c(INTERNAL, DISPLAY)) %>% 
            colnames() %>% 
            length
    })
    
    output$display_group_choice <- reactive(num_group_columns() > 1)
    outputOptions(output, "display_group_choice", suspendWhenHidden = FALSE)

    output$group_choice_ui <- renderUI({
        req(relationship_df())
        choices <- relationship_df() %>% 
            dplyr::select(-c(INTERNAL, DISPLAY)) %>% 
            colnames()
        selectInput(
            ns("y_group"),
            label = "Select Group",
            choices = choices)
    })
    
    output$variable_choice_ui <- renderUI({
        req(relationship_df(), num_group_columns())
        if(num_group_columns() == 1){
            choices  <- relationship_df() %>% 
                dplyr::select("INTERNAL", "DISPLAY", "CLASS" = 3) %>% 
                create_nested_list_by_class
        } else {
            req(input$y_group)
            choices  <- relationship_df() %>% 
                dplyr::select("INTERNAL", "DISPLAY", "CLASS" = input$y_group) %>% 
                create_nested_list_by_class
        }

        selectInput(
            ns("y_variable"),
            label = "Select Variable",
            choices = choices)
    })
    
    plot_df <- reactive({
        req(data_df(), input$y_variable, !is.null(input$log_scale))
        data_df() %>% 
            dplyr::select(x, y = input$y_variable, label) %>% 
            tidyr::drop_na() %>% 
            {`if`(
                input$log_scale, 
                dplyr::mutate(., y = log(y)),
                .
            )}
    })

    output$plot <- renderPlotly({
        req(plot_df(), input$plot_type)
        if(input$plot_type == "Violin") plot_func <- create_violinplot
        else if (input$plot_type == "Box") plot_func <- create_boxplot
        else stop("No plot selected")
        plot_func(
            plot_df(), 
            source_name = plot_source_name, 
            fill_colors = plot_colors(), 
            ...)
    })
    
    output$plot_text <- renderText({
        req(sample_group_df())
        create_group_text_from_plotly(
            plot_source_name,
            "",
            sample_group_df(),
            prompt_text = "",
            key_column = "x"
        )
    })
    
    output$histPlot <- renderPlotly({
        
        eventdata <- event_data("plotly_click", source = plot_source_name)
        validate(need(!is.null(eventdata), "Click plot above"))
        clicked_group <- eventdata$x[[1]]
        
        print(clicked_group)
        
        current_groups <- plot_df() %>% 
            magrittr::use_series(x) %>% 
            unique
        
        validate(need(clicked_group %in% current_groups, "Click plot above"))
        
        df <- plot_df() %>% 
            dplyr::filter(x == clicked_group) %>% 
            dplyr::select(x = y)
            
        print(df)
        create_histogram(df, title = clicked_group)
    })
    
    
    
    
    
    
    
    
    
    
    
}