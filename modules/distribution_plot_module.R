distributions_plot_module_UI <- function(
    id, 
    y_variable_options,
    y_variable_select_label = "Select plot Y Variable",
    title_text = "Distributions",
    message_text = stringr::str_c(
        "Select variable to its to see its distribution over sample groups.",
        "Plots are available as violin plots, and box plots with full data",
        "points superimposed."
    ),
    click_text = "Click plot to see group information."
    
){
    
    ns <- NS(id)
    
    sectionBox(
            title = title_text,
            messageBox(width = 12, p(message_text)),
            fluidRow(
                optionsBox(
                    width = 12,
                    column(
                        width = 8,
                        selectInput(
                            ns("y_variable"),
                            y_variable_select_label,
                            y_variable_options
                        )
                    ),
                    column(
                        width = 4,
                        selectInput(
                            ns("plot_type"),
                            "Select Plot Type",
                            choices = c("Violin", "Box")
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
            )
        )
}


distributions_plot_module <- function(
    input, 
    output, 
    session,
    plot_source_name,
    data_df,
    sample_group_df,
    plot_colors,
    ...
){
    plot_df <- reactive({
        data_df() %>% 
            dplyr::select(x, y = input$y_variable, label) %>% 
            tidyr::drop_na()
    })

    output$plot <- renderPlotly({
        if(input$plot_type == "Violin") plot_func <- create_violinplot
        else if (input$plot_type == "Box") plot_func <- create_boxplot
        else stop("No plot selected")
        plot_func(
            plot_df(), 
            source_name = plot_source_name, 
            fill_colors = plot_colors(), 
            ...)
    })
    
    output$plot_text <- renderText(
        create_group_text_from_plotly(
            plot_source_name,
            "",
            sample_group_df(),
            prompt_text = "",
            key_column = "x"
        )
    )
}