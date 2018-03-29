immunefeatures_UI <- function(id) {
    ns <- NS(id)
    
    tagList(
        titleBox("Immune Feature Trends"),
        textBox(
            width = 12,
            p("Some overview/summary text describing this module and the data presented within.")  
        ),
        sectionBox(
            title = "Distributions",
            messageBox(
                width = 12,
                p("Brief instructional message about this section, what to do in it, and the available options.")  
            ),
            fluidRow(
              optionsBox(
                width = 6,
                selectInput(
                  ns("violin_y"),
                  "Select violin plot y variable",
                  choices = get_friendly_numeric_columns(),
                  selected = "leukocyte_fraction"
                )
              )
            ),
            fluidRow(
              plotBox(
                width = 12,
                plotlyOutput(ns("violinPlot"))
              )
            )
        ),
        sectionBox(
          title = "Correlations",
          messageBox(
            width = 12,
            p("Brief instructional message about this section, what to do in it, and the available options.")  
          ),
          fluidRow(
            optionsBox(
              width = 12,
              column(
                width = 8,
                selectInput(
                  ns("heatmap_y"),
                  "Select heatmap y variable",
                  c(
                    "Core Expression Signature",
                    "DNA Alteration",
                    "Adaptive Receptor",
                    "T Helper Cell Score",
                    "Immune Cell Proportion - Original",
                    "Immune Cell Proportion - Aggregate 1",
                    "Immune Cell Proportion - Aggregate 2",
                    "Immune Cell Proportion - Aggregate 3"
                  ),
                  selected = "Immune Cell Proportion - Aggregate 2"
                )
              ),
              column(
                width = 4,
                selectInput(
                  ns("heatmap_values"),
                  "Select heatmap values",
                  c(
                    "Leukocyte Fraction" = "leukocyte_fraction",
                    "OS Time" = "OS_time",
                    "Mutation Rate, Non-Silent" = "mutationrate_nonsilent_per_Mb",
                    "Indel Neoantigens" = "indel_neoantigen_num",
                    "SNV Neoantigens" = "numberOfImmunogenicMutation",
                    "Stemness Score RNA" = "StemnessScoreRNA"
                  ),
                  selected = "Leukocyte Fraction"
                )
              )
            )
          ),
          fluidRow(
            plotBox(
              width = 6,
              plotlyOutput(ns("corrPlot"))
            ),
            plotBox(
              width = 6,
              plotlyOutput(ns("scatterPlot"))
            )
          )
        )
    )
}

immunefeatures <- function(input, output, session, ss_choice, subset_df) {
    ns <- session$ns
    
    ss_internal <- reactive(get_variable_internal_name(ss_choice()))
    sample_groups <- reactive(get_category_group(ss_internal()))

    output$violinPlot <- renderPlotly({
        
        display_x  <- ss_choice()
        display_y  <- input$violin_y
        internal_x <- get_variable_internal_name(display_x)
        internal_y <- get_variable_internal_name(display_y)
        
        plot_df <- subset_df() %>%
            select_(.dots = c(internal_x, internal_y)) %>%
            .[complete.cases(.),]
        
        plot_df %>% 
            create_violinplot(
                internal_x,
                internal_y,
                internal_x,
                xlab = display_x,
                ylab = display_y,
                fill_colors = decide_plot_colors(panimmune_data, internal_x)
            )
    })
    
    hm_variables  <- reactive(
        as.character(get_variable_group(input$heatmap_y))
    )
    intermediate_corr_df <- reactive(create_intermediate_corr_df(
        subset_df(),
        input$heatmap_values,
        ss_internal(),
        sample_groups(),
        hm_variables()
    ))
    
    output$corrPlot <- renderPlotly({
        heatmap_corr_mat <- create_heatmap_corr_mat(
            intermediate_corr_df(),
            input$heatmap_values,
            ss_internal(),
            sample_groups(),
            hm_variables()
        )
        
        create_plotly_heatmap(heatmap_corr_mat, "heatplot")
    })
    
    output$scatterPlot <- renderPlotly({
        eventdata <- event_data("plotly_click", source = "heatplot")
        validate(need(!is.null(eventdata), "Click heatmap"))
        
        internal_variable_name <- eventdata$y[[1]] %>%
            get_variable_internal_name() %>%
            .[. %in% colnames(intermediate_corr_df())]
        
        plot_df <- intermediate_corr_df() %>% 
            create_scatterplot_df(
                filter_column = ss_internal(),
                filter_value = eventdata$x[[1]],
                x_column = internal_variable_name,
                y_column = input$heatmap_values
            )
        
        plot_df %>%
            create_scatterplot(
                x_column = internal_variable_name,
                y_column = input$heatmap_values,
                x_lab = eventdata$y[[1]],
                y_lab = get_variable_display_name(input$heatmap_values),
                title = eventdata$x[[1]])
    })
}

