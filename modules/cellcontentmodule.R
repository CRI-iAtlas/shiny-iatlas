# UI ----
cellcontent_UI <- function(id) {
    ns <- NS(id)
    
    tagList(
        titleBox("Tumor Composition"),
        textBox(
            width = 12,
            p("Explore the immune cell proportions in your sample groups.")  
        ),
        
        # Overall proportions section ----
        sectionBox(
            title = "Overall Cell Proportions",
            messageBox(
                width = 12,
                p("The barplots show the mean proportion of the tumor fraction, overall stromal fraction (one minus tumor fractions) and the leukocyte fraction of samples with each group.  Error bars show standard error of the mean.")
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
                p("Click on the bars for a sample group and you will get generate a scatter plot, showing leukocyte fraction on the Y-axis and stromal fraction on the X-axis. Points near the diagonal correspond to tumor samples in which non-tumor stromal cells are nearly all immune cells, and points away from the diagonal correspond to a more mixed or a non-immune stromal tumor microenvironment.  Points in the upper-left triangle of the plot are estimation artifacts."),
                p("Manuscript context:  Looking at TCGA tumor types, select PRAD and then SKCM and you will get what corresponds to Figure 2C.")
            ),
            fluidRow(
                
                # ** Overall proportions correlation plots ----
                plotBox(
                    width = 12,
                    column(
                        width = 6,
                        
                        plotlyOutput(ns("lf_sf_corr_scatterplot"))
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
                operations = c("mean", "se")
            ) %>% 
            create_barplot(
                x_column = ss_internal(),
                y_column = "mean", 
                color_column = "fraction_name",
                error_column = "se",
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
    
    # Cell fractions logic ----
    
    # ** Cell fractions bar plot render ----
    output$cell_frac_barplot <- renderPlotly({
        
        cell_fractions <- as.character(get_variable_group(input$cf_choice))
        subset_df() %>%
            create_cell_fraction_df(
                group_column = ss_internal(), 
                cell_fraction_columns = cell_fractions
            ) %>%
            create_barplot_df(
                value_column = "fraction",
                group_column = "fraction_name",
                subgroup_column = ss_internal(),
                operations = c("mean", "se")
            ) %>% 
            mutate(fraction_name =  map_chr(fraction_name, get_variable_display_name)) %>% 
            create_barplot(
                x_column = ss_internal(),
                y_column = "mean",
                color_column = "fraction_name",
                error_column = "se",
                x_lab = "Fraction type by group",
                y_lab = "Fraction mean",
                source_name = "cell_frac_barplot"
            )
    })
}

