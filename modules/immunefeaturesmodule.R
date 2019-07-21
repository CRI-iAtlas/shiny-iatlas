immunefeatures_UI <- function(id) {
    ns <- NS(id)
    
    tagList(
        titleBox("iAtlas Explorer — Immune Feature Trends"),
        textBox(
            width = 12,
            p(stringr::str_c(
                "This module allows you to see how immune readouts vary",
                "across your groups, and how they relate to one another."
            ))  
        ),
        distributions_plot_module_UI(
            ns("dist"),
            message_html = p(stringr::str_c(
                "This displays the value of immune readouts by sample group.",
                "Select a variable class to see the distribution of variables",
                "within that class displayed as as violin plot. Manuscript",
                "context: This allows you to display distributions such as",
                "those shown in Figures 1C and 1D.",
                sep = " "
            )),
        ),
    
        sectionBox(
            title = "Correlations",
            messageBox(
                width = 12,
                p(stringr::str_c(
                    "Here, you can look at correlation of a response variable",
                    "with other variables, within each sample group.  Select",
                    "the response variable on the right. Select a variable",
                    "class on the left to specify which other variable you",
                    "would like to correlate the response variable with. The",
                    "result will be a heatmap, with positive correlation",
                    "shown with a red scale, absence of correlation in white,",
                    "and negative correlation in blue.  Click on any cell in",
                    "the heatmap to see the underlying data as a scatterplot.",
                    "In the scatterplot, each point represents a tumor sample,",
                    "the response variable is shown on the Y-axis and the row",
                    "variable is shown on the X-axis.",
                    sep = " "
                )),
                p(stringr::str_c(
                    "Manuscript context:  Select “Leukocyte Fraction” as the",
                    "response variable, “DNA Alteration” as the variable",
                    "class, and Spearman correlation. This will correspond to",
                    "Figure 4A if you are looking at immune subtypes as your",
                    "sample grouping.",
                    sep = " "
                ))
            ),
            fluidRow(
                optionsBox(
                    width = 12,
                    column(
                        width = 8,
                        selectInput(
                            ns("heatmap_y"),
                            "Select Variable Class",
                            get_numeric_classes_from_feature_df(),
                            selected = "Immune Cell Proportion - Original"
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
                    ),
                    column(
                        width = 4,
                        selectInput(
                            ns("correlation_method"),
                            "Select Correlation Method",
                            choices = unlist(config_yaml$correlation_methods),
                            selected = "Spearman"
                        )
                    )
                )
            ),
            fluidRow(
                plotBox(
                    width = 12,
                    fluidRow(
                        plotlyOutput(ns("heatmap")) %>% 
                            shinycssloaders::withSpinner(),
                        p(),
                        textOutput(ns("heatmap_group_text"))
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
    input,
    output, 
    session, 
    group_display_choice, 
    group_internal_choice, 
    sample_group_df,
    subset_df, 
    plot_colors
){
    data_df <- reactive({
        dplyr::select(
            subset_df(),
            x = group_internal_choice(),
            label = "ParticipantBarcode",
            dplyr::everything()) 
    })
    
    relationship_df <- reactive({
        panimmune_data$feature_df %>% 
            dplyr::filter(VariableType == "Numeric") %>% 
            dplyr::select(
                INTERNAL = FeatureMatrixLabelTSV, 
                DISPLAY = FriendlyLabel,
                `Variable Class`)
    })
    
    callModule(
        distributions_plot_module,
        "dist",
        "immunefeatures_dist_plot",
        data_df,
        relationship_df,
        sample_group_df,
        plot_colors,
        group_display_choice,
        key_col = "label"
    )

    hm_variables  <- reactive({
        get_factored_variables_from_feature_df(input$heatmap_y) %>% 
            as.character
    })
    
    immunefeatures_df <- reactive({
        
        req(!is.null(subset_df()), cancelOutput = T)
        
        sample_groups <- get_unique_column_values(
            group_internal_choice(), 
            subset_df())
        
        build_immunefeatures_df(
            subset_df(),
            group_column = group_internal_choice(),
            value1_column = input$heatmap_values,
            value2_columns = hm_variables(),
            group_options = sample_groups)
        
    })
        
    
    
    output$heatmap <- renderPlotly({
        
        validate(
            need(nrow(immunefeatures_df()) > 0, 
                 "Current selected group and selected variable have no overlap")
        )
        
        immunefeatures_correlation_matrix <- build_immunefeatures_correlation_matrix(
                immunefeatures_df(), 
                input$correlation_method)
        
        create_heatmap(immunefeatures_correlation_matrix, "heatplot", scale_colors = T)
    })
    
    output$heatmap_group_text <- renderText({
        req(group_internal_choice(), sample_group_df(), cancelOutput = T)
        
        create_group_text_from_plotly(
            "heatplot", 
            group_internal_choice(), 
            sample_group_df(),
            key_column = "x")
    })
        
    
    output$scatterPlot <- renderPlotly({
        
        req(!is.null(subset_df()), cancelOutput = T)
        
        eventdata <- event_data("plotly_click", source = "heatplot")
        
        validate(need(
            check_immunefeatures_scatterplot_click_data(
                eventdata, 
                subset_df(), 
                group_internal_choice(), 
                immunefeatures_df()),
            "Click above heatmap"))
        
        
        internal_variable_name <- eventdata$y[[1]] %>%
            get_variable_internal_names() %>%
            .[. %in% colnames(immunefeatures_df())]

        scatterplot_df <- build_immunefeatures_scatter_plot_df(
            immunefeatures_df(),
            x_col = internal_variable_name,
            group_filter_value = eventdata$x[[1]]
        )
        
        create_scatterplot(
            scatterplot_df,
            xlab = eventdata$y[[1]],
            ylab = get_variable_display_name(input$heatmap_values),
            title = eventdata$x[[1]],
            label_col = "label")
    })
}
