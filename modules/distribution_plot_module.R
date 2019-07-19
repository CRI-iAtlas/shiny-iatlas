distributions_plot_module_UI <- function(id){
    
    ns <- NS(id)
    
    sectionBox(
            title = "TIL Map Characteristics",
            messageBox(
                width = 12,
                p("Select a TIL map characteristic to see its distribution over sample groups. Plots are available as violin plots, and box plots with full data points superimposed."),
                p("Main immune manuscript context:  If you are looking at immune subtypes, select TIL Regional Fraction to get Figure 3B.")
            ),
            fluidRow(
                optionsBox(
                    width = 12,
                    column(
                        width = 4,
                        selectInput( ## would be good to initiate on til_percentage/"TIL Regional Fraction (Percent)"
                            ns("y_variable"),
                            "Select TIL Map Characteristic",
                            get_tilmap_nested_list()
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
                    h4("Click point or violin/box to filter samples in table below")
                )
            )
        )
}


distributions_plot_module <- function(
    input, 
    output, 
    session,
    data_df,
    sample_group_df,
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
        plot_func(plot_df(), source_name = "plot", ...)
    })
    
    sample_clicked <- reactive(get_eventdata_from_plotly("plotly_click", "plot"))
        
    plot_text   <- reactive(create_group_text_from_plotly(
        "plot",
        "",
        sample_group_df(),
        prompt_text = "",
        key_column = "x"
    ))
    
    output$plot_text <- renderText(plot_text())
    return(sample_clicked)
}