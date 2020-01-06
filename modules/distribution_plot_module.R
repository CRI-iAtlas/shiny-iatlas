distributions_plot_module_UI <- function(
    id, 
    title_text = "Distributions",
    message_html = p(stringr::str_c(
        "Select variable to its to see its distribution over sample groups.",
        "Plots are available as violin plots, and box plots with full data",
        "points superimposed."
    )),
    click_text = "Click plot to see group information.",
    scale_default = "None",
    scale_options = c(
        "None", 
        "Log2", 
        "Log2 + 1",
        "Log10",
        "Log10 + 1"
    ),
    plot_clicked_group_default = F
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
                        selectInput(
                            ns("scale_method"), 
                            "Select variable scaling", 
                            selected = scale_default,
                            choices = scale_options
                        )
                    ),
                    column(
                        width = 4,
                        checkboxInput(
                            ns("see_drilldown"), 
                            "Plot clicked group?", 
                            plot_clicked_group_default
                        )
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
            downloadButton(ns('download_data'), 'Download'),
            conditionalPanel(
                condition =  "input.see_drilldown",
                ns = ns,
                fluidRow(
                    plotBox(
                        width = 12,
                        plotlyOutput(ns("drilldown_plot")) %>% 
                            shinycssloaders::withSpinner()
                    )
                )
            )
        )
}



distributions_plot_module <- function(
    input, 
    output, 
    session,
    plot_source_name,
    feature_values_tbl,
    feature_metadata_tbl,
    sample_tbl,
    groups_tbl,
    group_display_choice,
    plot_colors,
    variable_group_names = NULL,
    variable_selection_default = NA,
    ...
){
    
    ns <- session$ns
    
    # determines if there are multiple ways to group input variables
    multiple_variable_columns <- reactive({
        req(feature_metadata_tbl())
        num_group_columns <- feature_metadata_tbl() %>% 
            dplyr::select(-c(INTERNAL, DISPLAY)) %>% 
            colnames() %>% 
            length
        num_group_columns > 1
    })
    
    # This is so that the conditional panel can see output$display_group_choice
    output$display_group_choice <- reactive(multiple_variable_columns())
    outputOptions(output, "display_group_choice", suspendWhenHidden = FALSE)
    
    # used when feature_metadata_con has more than one grouping column
    output$group_choice_ui <- renderUI({
        req(feature_metadata_tbl())
        choices <- feature_metadata_tbl() %>% 
            dplyr::select(-c(INTERNAL, DISPLAY)) %>% 
            colnames()
        if(!is.null(variable_group_names)){
            names(choices) <- variable_group_names
        }
        selectInput(
            ns("group_choice"),
            label = "Select Group",
            choices = choices)
    })
    
    # used to determine what column to use for group choices
    variable_choice_class_column <- reactive({
        if(multiple_variable_columns()){
            req(input$group_choice)
            return(input$group_choice)
        } else{
            return(3)
        } 
    })
    
    output$variable_choice_ui <- renderUI({
        req(variable_choice_class_column(), feature_metadata_tbl())
        selectInput(
            ns("variable_choice_id"),
            label = "Select Variable",
            selected = variable_selection_default,
            choices = create_nested_named_list(
                feature_metadata_tbl(),
                names_col1 = variable_choice_class_column(),
                names_col2 = "DISPLAY",
                values_col = "INTERNAL"
            )
        )
    })
    
    distribution_plot_tbl <- reactive({
        req(
            feature_values_tbl(),
            input$variable_choice_id,
            input$scale_method,
            sample_tbl()
        )
        
        feature_values_tbl() %>%  
            dplyr::filter(feature_id == local(input$variable_choice_id)) %>% 
            scale_db_connection(input$scale_method) %>% 
            dplyr::inner_join(sample_tbl(), by = "sample_id") %>% 
            dplyr::select(label = sample_id, x = group, y = value)
        
    })
    
    varible_display_name <- reactive({
        feature_metadata_tbl() %>% 
            dplyr::filter(INTERNAL == local(input$variable_choice_id)) %>% 
            dplyr::pull(DISPLAY)
    })
    
    varible_plot_label <- reactive({
        add_transformation_string_to_feature(
            input$scale_method,  
            varible_display_name()
        )
    })
    
    plot_function <- reactive({
        switch(
            input$plot_type,
            "Violin" = create_violinplot,
            "Box" = create_boxplot
        )
    })
    
    output$plot <- renderPlotly({
        plot_function()(
            distribution_plot_tbl(),
            xlab = group_display_choice(),
            ylab = varible_plot_label(),
            title = varible_display_name(),
            source_name = plot_source_name, 
            fill_colors = plot_colors(), 
            ...)
    })
    
    output$plot_text <- renderText({
        req(groups_tbl)
        eventdata <- event_data("plotly_click", source = plot_source_name)
        validate(need(eventdata, "Click above plot"))
        
        groups_tbl() %>% 
            dplyr::filter(group == local(unique(dplyr::pull(eventdata, "x")))) %>% 
            dplyr::mutate(text = paste0(name, ": ", characteristics)) %>%
            dplyr::pull(text)

    })
    
    output$download_data <- downloadHandler(
        filename = function() stringr::str_c("data-", Sys.Date(), ".csv"),
        content = function(con) readr::write_csv(plot_df(), con)
    )
    
    output$drilldown_plot <- renderPlotly({
        
        eventdata <- event_data("plotly_click", source = plot_source_name)
        validate(need(!is.null(eventdata), "Click plot above"))
        clicked_group <- eventdata$x[[1]]
        
        
        current_groups <- distribution_plot_tbl() %>% 
            dplyr::pull(x) %>% 
            unique
        
        validate(need(clicked_group %in% current_groups, "Click plot above"))
        
        distribution_plot_tbl() %>% 
            dplyr::filter(x == clicked_group) %>% 
            dplyr::select(x = y) %>% 
            dplyr::as_tibble() %>% 
            create_histogram(
                title = clicked_group, 
                x_lab = varible_plot_label()
            )
    })
}