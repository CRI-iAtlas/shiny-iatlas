distributions_plot_module_UI <- function(
    id, 
    title_text = "Distributions",
    message_html = p(stringr::str_c(
        "Select variable to its to see its distribution over sample groups.",
        "Plots are available as violin plots, and box plots with full data",
        "points superimposed. For reordering violins, first choose a variable (bar) to sort on, then a sorting function like Mean or Median. Reordering function Max sorts by the maximum value and min by the minimum value within each group."
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
                            "Display histogram of distribution by clicking on a violin", 
                            plot_clicked_group_default
                        )
                    ),
                    column(
                        width = 4,
                        selectInput(
                            ns("reorder_distributions"), 
                            "Reorder Function", 
                            choices=c('None','Median','Mean','Max','Min'),
                            selected = 'None'
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

# data_df
# Each row respresents a sample
# 1st col: "x", contains membership of current group selection, ie. "C1"
# All other cols: numeric values 
# metadata_df
# Each row represents a variable, and of the columns in data_df
# 1st col: "INTERNAL", internal name for variable
# 2nd col: "DISPLAY", variable name used in the ui
# All other cols: each column is a grouping of variables, to organize selection

distributions_plot_module <- function(
    input, 
    output, 
    session,
    plot_source_name,
    data_df,
    metadata_df,
    group_metadata_df,
    plot_colors,
    group_display_choice,
    variable_selection_default = NA,
    ...
){
    
    ns <- session$ns
    
    multiple_variable_columns <- reactive({
        req(metadata_df())
        num_group_columns <- metadata_df() %>% 
            dplyr::select(-c(INTERNAL, DISPLAY)) %>% 
            colnames() %>% 
            length
        num_group_columns > 1
    })
    
    # This is so that the conditional panel can see output$display_group_choice
    output$display_group_choice <- reactive(multiple_variable_columns())
    outputOptions(output, "display_group_choice", suspendWhenHidden = FALSE)

    output$group_choice_ui <- renderUI({
        req(metadata_df())
        choices <- metadata_df() %>% 
            dplyr::select(-c(INTERNAL, DISPLAY)) %>% 
            colnames()
        selectInput(
            ns("group_choice"),
            label = "Select or Search Group",
            choices = choices)
    })
    

    output$variable_choice_ui <- renderUI({
        req(metadata_df())

        if(multiple_variable_columns()){
            req(input$group_choice)
            variable_column <- input$group_choice  
        } else{
            variable_column <- 3  
        } 
        
        choices  <- metadata_df() %>% 
            dplyr::select("INTERNAL", "DISPLAY", "CLASS" = variable_column) %>% 
            create_nested_list_by_class()

        selectInput(
            ns("variable_choice"),
            label = "Select or Search for Variable",
            choices = choices,
            selected = variable_selection_default)
    })
    
    plot_df <- reactive({
        req(data_df(), 
            input$variable_choice, 
            input$scale_method)
        build_distribution_plot_df2(  
            data_df(), 
            input$variable_choice, 
            input$scale_method,
            input$reorder_distributions,
            group_display_choice())
    })
    
    varible_display_name <- reactive({
        convert_value_between_columns(
            input$variable_choice, 
            metadata_df(),
            "INTERNAL",
            "DISPLAY")
    })
    
    varible_plot_label <- reactive({
        switch(
            input$scale_method,
            "None" = varible_display_name(),
            
            "Log2" = stringr::str_c(
                "Log2( ", 
                varible_display_name(),
                " )"),
            
            "Log2 + 1" = stringr::str_c(
                "Log2( ", 
                varible_display_name(),
                " + 1 )"),
            
            "Log10" = stringr::str_c(
                "Log10( ", 
                varible_display_name(), 
                " )"),
            
            "Log10 + 1" = stringr::str_c(
                "Log10( ", 
                varible_display_name(), 
                " + 1 )")
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
            plot_df(), 
            xlab = group_display_choice(),
            ylab = varible_plot_label(),
            title = varible_display_name(),
            source_name = plot_source_name, 
            fill_colors = plot_colors(), 
            ...)
    })
    
    output$plot_text <- renderText({
        req(group_metadata_df())
        create_group_text_from_plotly(
            plot_source_name,
            "",
            group_metadata_df(),
            prompt_text = "",
            key_column = "x"
        )
    })
    
    output$download_data <- downloadHandler(
        filename = function() stringr::str_c("data-", Sys.Date(), ".csv"),
        content = function(con) readr::write_csv(plot_df(), con)
    )
    
    output$drilldown_plot <- renderPlotly({
        
        eventdata <- event_data("plotly_click", source = plot_source_name)
        validate(need(!is.null(eventdata), "Click plot above"))
        clicked_group <- eventdata$x[[1]]
        
        
        current_groups <- plot_df() %>% 
            magrittr::use_series(x) %>% 
            unique
        
        validate(need(clicked_group %in% current_groups, "Click plot above"))
        
        df <- plot_df() %>% 
            dplyr::filter(x == clicked_group) %>% 
            dplyr::select(x = y) %>% 
            create_histogram(
                title = clicked_group, 
                x_lab = varible_plot_label()
            )
    })
}