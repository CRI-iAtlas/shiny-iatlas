# UI ----
cellcontent_UI <- function(id) {
    ns <- NS(id)
    
    tagList(
        titleBox("Tumor Composition"),
        
        # Overall proportions section ----
        sectionBox(
            title = "Overall Cell Proportions",
            fluidRow(
                
                # ** Overall proportions bar plot ----
                plotBox(
                    width = 12,
                    plotlyOutput(ns("overall_props_barplot"))
                )
            ),
            fluidRow(
                
                # ** Overall proportions correlation plots ----
                plotBox(
                    width = 12,
                    column(
                        width = 6,
                        
                        plotlyOutput(ns("lf_sf_corr_scatterplot"))
                    ),
                    column(
                        width = 6,
                        plotlyOutput(ns("lf_tf_corr_scatterplot"))
                    )
                )
            )
        ),
        
        # Cell fractions section ----
        sectionBox(
            title = "Cell Type Fractions",
            fluidRow(
                # optionsBox(
                #     width = 8,
                #     selectInput(
                #         inputId = ns("cc_choice"),
                #         label = "Select Cellular Content",
                #         choices = as.character(
                #             panimmune_data$cell_content_choices
                #         ),
                #         selected = "Leukocyte Fraction"
                #     )
                # )
            ),
            fluidRow(
                
                # ** Cell fractions bar plot ----
                plotBox(
                    width = 12
                    # plotOutput(ns("distPlot"))
                )
            )
        )
    )
}

# Server ----
cellcontent <- function(input, output, session, ss_choice, subset_df) {
    
    ss_group <- reactive(get_variable_internal_name(ss_choice()))
    
    # Overall proportions logic ----
    plot_colors <- reactive(decide_plot_colors(panimmune_data, ss_group()))
    
    # ** Overall proportions bar plot render ----
    output$overall_props_barplot <- renderPlotly({
        subset_df() %>% 
        create_tumor_content_df(group_column = ss_group()) %>% 
            create_barplot_df(
                value_column = "fraction",
                group_column = "fraction_name",
                subgroup_column = ss_group(),
                operations = c("mean", "sd")
            ) %>% 
            create_barplot(
                x_column = ss_group(),
                y_column = "mean", 
                color_column = "fraction_name",
                error_column = "sd",
                x_lab = "Fraction type by group",
                y_lab = "Fraction mean"
            )
    })
    
    # ** Overall proportions scatter plot renders ----
    output$lf_sf_corr_scatterplot <- renderPlotly({
        # eventdata <- event_data(
        #     "plotly_click", source = "overall_props_barplot"
        # )
        # validate(need(!is.null(eventdata), "Click bar plot"))

        subset_df() %>%
            create_scatterplot_df(
                filter_column = "Subtype_Immune_Model_Based", # ss_internal(),
                filter_value = "C1", # eventdata$x[[1]],
                x_column = "leukocyte_fraction",
                y_column = "Stromal_Fraction"
            ) %>%
            create_scatterplot(
                x_column = "leukocyte_fraction",
                y_column = "Stromal_Fraction",
                x_lab = "Leukocyte Fraction",
                y_lab = "Stromal Fraction",
                title = "C1" # eventdata$x[[1]]
            )
    })
    
    output$lf_tf_corr_scatterplot <- renderPlotly({
        # eventdata <- event_data(
        #     "plotly_click", source = "overall_props_barplot"
        # )

        subset_df() %>%
            mutate(Tumor_Fraction = 1 - Stromal_Fraction) %>% 
            create_scatterplot_df(
                filter_column = "Subtype_Immune_Model_Based", # ss_internal(),
                filter_value = "C1", # eventdata$x[[1]],
                x_column = "leukocyte_fraction",
                y_column = "Tumor_Fraction"
            ) %>%
            create_scatterplot(
                x_column = "leukocyte_fraction",
                y_column = "Tumor_Fraction",
                x_lab = "Leukocyte Fraction",
                y_lab = "Tumor Fraction",
                title = "C1" # eventdata$x[[1]]
            )
    })
    
    # Cell fractions logic ----
    
    # ** Cell fractions bar plot render ----
    # output$distPlot <- renderPlot({
    #     cc_group <- get_variable_internal_name(input$cc_choice)
    #     plot_df <- create_tumor_content_df(subset_df(), ss_group(), cc_group)
    #    
    #     plot <- create_boxplot(
    #         plot_df,
    #         x = ss_group(),
    #         y = cc_group,
    #         fill_factor = ss_group(),
    #         x_label = ss_choice(),
    #         y_label = input$cc_choice,
    #         fill_colors = plot_colors()
    #     )
    #     print(plot)
    # })
}