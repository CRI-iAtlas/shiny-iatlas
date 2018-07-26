immunefeatures_UI <- function(id) {
    ns <- NS(id)
    
    tagList(
        titleBox("iAtlas Explorer — Immune Feature Trends"),
        textBox(
            width = 12,
            p("This module allows you to see how immune readouts vary across your groups, and how they relate to one another.")  
        ),
        sectionBox(
            title = "Distributions",
            messageBox(
                width = 12,
                p("This displays the value of immune readouts by sample group. Select a variable class to see the distribution of variables within that class displayed as as violin plot."),
                p("Manuscript context: This allows you to display distributions such as those shown in Figures 1C and 1D.")
            ),
            fluidRow(
                optionsBox(
                    width = 6,
                    selectInput(
                        ns("violin_y"),
                        "Select Violin Plot Y Variable",
                        choices = get_feature_df_nested_list()
                    )
                )
            ),
            fluidRow(
                plotBox(
                    width = 12,
                    plotlyOutput(ns("violinPlot")) %>% 
                        shinycssloaders::withSpinner()
                )
            )
        ),
        sectionBox(
            title = "Correlations",
            messageBox(
                width = 12,
                p("Here, you can look at correlation of a response variable with other variables, within each sample group.  Select the response variable on the right. Select a variable class on the left to specify which other variable you would like to correlate the response variable with. The result will be a heatmap, with positive correlation shown with a red scale, absence of correlation in white, and negative correlation in blue.  Click on any cell in the heatmap to see the underlying data as a scatterplot. In the scatterplot, each point represents a tumor sample, the response variable is shown on the Y-axis and the row variable is shown on the X-axis.
"),
                p("Manuscript context:  Select “Leukocyte Fraction” as the response variable “DNA Alteration” as the variable class. This will correspond to Figure 4A if you are looking at immune subtypes as your sample grouping.")
            ),
            fluidRow(
                optionsBox(
                    width = 12,
                    column(
                        width = 8,
                        selectInput(
                            ns("heatmap_y"),
                            "Select Variable Class",
                            c(
                                "Core Expression Signature",
                                "DNA Alteration",
                                "Adaptive Receptor - B cell",
                                "Adaptive Receptor - T cell",
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
                            "Select Response Variable",
                            choices = get_feature_df_nested_list(),
                            selected = "Leukocyte Fraction"
                        )
                    )
                )
            ),
            fluidRow(
                plotBox(
                    width = 12,
                    fluidRow(
                        plotlyOutput(ns("corrPlot")) %>% 
                            shinycssloaders::withSpinner()
                    )
                )
            ),
            fluidRow(
                plotBox(
                    width = 12,
                    plotlyOutput(ns("scatterPlot")) %>%
                        shinycssloaders::withSpinner()
                )
            )
        )
    )
}

# Server ----
immunefeatures <- function(
    input, output, session, group_display_choice, group_internal_choice, 
    subset_df, plot_colors) {
    
    ns <- session$ns
    
    sample_groups <- reactive(
        get_unique_column_values(
            group_internal_choice(), 
            subset_df()))
    
    output$violinPlot <- renderPlotly({
        display_x  <- group_display_choice()
        internal_x <- group_internal_choice()
        internal_y <- input$violin_y
        display_y  <- get_variable_display_name(internal_y)
        
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
                fill_colors = plot_colors()
            )
    })
    
    hm_variables  <- reactive({
        get_factored_variables_from_feature_df(input$heatmap_y) %>% 
            as.character
    })
    
    intermediate_corr_df <- reactive({
        df <- subset_df() %>% 
            build_intermediate_corr_df(
                value_column = input$heatmap_values,
                group_column = group_internal_choice(),
                group_options = sample_groups(),
                corr_value_columns = hm_variables())
        return(df)
    })
    
    output$corrPlot <- renderPlotly({
        heatmap_corr_mat <- intermediate_corr_df() %>%
            build_heatmap_corr_mat(
                value_column = input$heatmap_values,
                group_column = group_internal_choice(),
                group_options = sample_groups(),
                corr_value_columns = hm_variables()
            )
        create_heatmap(heatmap_corr_mat, "heatplot")
    })
    
    output$scatterPlot <- renderPlotly({
        eventdata <- event_data("plotly_click", source = "heatplot")
        
        validate(need(
            check_immunefeatures_scatterplot_click_data(
                eventdata, 
                subset_df(), 
                group_internal_choice(), 
                intermediate_corr_df()),
            "Click above heatmap"))
        
        
        internal_variable_name <- eventdata$y[[1]] %>%
            get_variable_internal_name() %>%
            .[. %in% colnames(intermediate_corr_df())]
        
        
        plot_df <- intermediate_corr_df() %>% 
            build_scatterplot_df(
                filter_column = group_internal_choice(),
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
