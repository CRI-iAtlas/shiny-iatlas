# UI ----
cellcontent_UI <- function(id) {
    ns <- NS(id)
    
    tagList(
        titleBox("Tumor Composition"),
        textBox(
            width = 12,
            p("Some overview/summary text describing this module and the data presented within.")  
        ),
        
        # Overall proportions section ----
        sectionBox(
            title = "Overall Cell Proportions",
            messageBox(
                width = 12,
                p("Brief instructional message about this section, what to do in it, and the available options.")  
            ),
            fluidRow(
                
                # ** Overall proportions bar plot ----
                plotBox(
                    width = 12,
                    plotlyOutput(ns("overall_props_barplot"))
                )
            ),
            messageBox(
                width = 12,
                p("Click on the bars for a sample group (x-axis) to view correlation between leukocyte fraction and other components.")  
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
            messageBox(
                width = 12,
                p("Brief instructional message about this section, what to do in it, and the available options.")  
            ),
            fluidRow(
                optionsBox(
                    width = 8,
                    selectInput(
                        inputId = ns("cf_choice"),
                        label = "Select Cell Fraction Type",
                        choices = config_yaml$cell_type_aggregates,
                        selected = config_yaml$cell_type_aggregates[[1]]
                    )
                )
            ),
            fluidRow(
                
                # ** Cell fractions bar plot ----
                plotBox(
                    width = 12,
                    plotlyOutput(ns("cell_frac_barplot"))
                )
            )
        )
    )
}

# Server ----
cellcontent <- function(input, output, session, ss_choice, subset_df) {
    
    ss_internal <- reactive(get_variable_internal_name(ss_choice()))
    
    # Overall proportions logic ----
    plot_colors <- reactive(decide_plot_colors(panimmune_data, ss_internal()))
    
    # ** Overall proportions bar plot render ----
    output$overall_props_barplot <- renderPlotly({
        subset_df() %>% 
        create_tumor_content_df(group_column = ss_internal()) %>% 
            create_barplot_df(
                value_column = "fraction",
                group_column = "fraction_name",
                subgroup_column = ss_internal(),
                operations = c("mean", "sd")
            ) %>% 
            create_barplot(
                x_column = ss_internal(),
                y_column = "mean", 
                color_column = "fraction_name",
                error_column = "sd",
                x_lab = "Fraction type by group",
                y_lab = "Fraction mean",
                source_name = "overall_props_barplot"
            )
    })
    
    # ** Overall proportions scatter plot renders ----
    output$lf_sf_corr_scatterplot <- renderPlotly({
        eventdata <- event_data(
            "plotly_click", source = "overall_props_barplot"
        )
        validate(need(!is.null(eventdata), "Click bar plot"))
        selected_plot_subgroup <- eventdata$x[[1]]

        subset_df() %>%
            create_scatterplot_df(
                filter_column = ss_internal(),
                filter_value = selected_plot_subgroup,
                x_column = "leukocyte_fraction",
                y_column = "Stromal_Fraction"
            ) %>%
            create_scatterplot(
                x_column = "leukocyte_fraction",
                y_column = "Stromal_Fraction",
                x_lab = "Leukocyte Fraction",
                y_lab = "Stromal Fraction",
                title = selected_plot_subgroup
            )
    })
    
    output$lf_tf_corr_scatterplot <- renderPlotly({
        eventdata <- event_data(
            "plotly_click", source = "overall_props_barplot"
        )
        validate(need(!is.null(eventdata), "Click bar plot"))
        selected_plot_subgroup <- eventdata$x[[1]]

        subset_df() %>%
            mutate(Tumor_Fraction = 1 - Stromal_Fraction) %>% 
            create_scatterplot_df(
                filter_column = ss_internal(),
                filter_value = selected_plot_subgroup,
                x_column = "leukocyte_fraction",
                y_column = "Tumor_Fraction"
            ) %>%
            create_scatterplot(
                x_column = "leukocyte_fraction",
                y_column = "Tumor_Fraction",
                x_lab = "Leukocyte Fraction",
                y_lab = "Tumor Fraction",
                title = selected_plot_subgroup
            )
    })
    
    # Cell fractions logic ----
    
    # ** Cell fractions bar plot render ----
    output$cell_frac_barplot <- renderPlotly({
        
        cell_fractions <- as.character(get_cell_content_group(input$cf_choice))
        print(cell_fractions)
        subset_df() %>%
            create_cell_fraction_df(
                group_column = ss_internal(), 
                cell_fraction_columns = cell_fractions
            ) %>%
            create_barplot_df(
                value_column = "fraction",
                group_column = "fraction_name",
                subgroup_column = ss_internal(),
                operations = c("mean", "sd")
            ) %>%
            create_barplot(
                x_column = ss_internal(),
                y_column = "mean",
                color_column = "fraction_name",
                error_column = "sd",
                x_lab = "Fraction type by group",
                y_lab = "Fraction mean",
                source_name = "cell_frac_barplot"
            )
    })
}

# df <- panimmune_data$df %>%
#     create_cell_fraction_df(
#         group_column = "Subtype_Immune_Model_Based",
#         cell_fraction_columns = as.character(get_cell_content_group("Immune Cell Proportion - Original"))
#         ) %>% 
#     create_barplot_df(
#         value_column = "fraction",
#         group_column = "fraction_name",
#         subgroup_column = "Subtype_Immune_Model_Based",
#         operations = c("mean", "sd"))
    
# 
# 
# x <- let(
#     alias = c(group_col = "Subtype_Immune_Model_Based"),
#     panimmune_data$df %>%
#         select(group_col, cell_fraction_columns) %>%
#         .[complete.cases(.), ] %>% 
#         gather(fraction_name, fraction, -group_col)) #%>% 
#     # TODO: this is really slow... need to fix
#     # mutate(fraction_name = map_chr(fraction_name, get_variable_display_name))
# )
#     

