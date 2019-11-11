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
            message_html = includeMarkdown("data/markdown/immune_features_dist.markdown"),
        ),
        immunefeatures_correlations_ui(ns("immunefeatures_correlations"))
    )
}

# Server ----
immunefeatures <- function(
    input,
    output, 
    session, 
    group_display_choice, 
    group_con,
    feature_values_con,
    features_con,
    plot_colors
){
    
    distributions_feature_value_con <- reactive({
        req(feature_values_con())
        feature_values_con() %>% 
            dplyr::select(label = sample, x = group, feature, y = value) 
    })
    

    distributions_feature_con <- reactive({
        req(features_con())
        features_con() %>% 
            dplyr::select(DISPLAY = display, INTERNAL = feature, CLASS = class) %>% 
            dplyr::filter_all(dplyr::all_vars(!is.na(.)))
    })
    
    callModule(
        distributions_plot_module,
        "dist",
        "immunefeatures_dist_plot",
        distributions_feature_value_con,
        distributions_feature_con,
        group_con,
        group_display_choice,
        plot_colors,
        key_col = "label"
    )
    
    ### immune features correlations
    
    callModule(
        immunefeatures_correlations,
        "immunefeatures_correlations",
        feature_values_con,
        features_con,
        group_con
    )

}

immunefeatures_correlations_ui <- function(id) {
    ns <- NS(id)
    
    tagList()
    
    sectionBox(
        title = "Correlations",
        messageBox(
            width = 12,
            includeMarkdown("data/markdown/immune_features_correlations.markdown")
        ),
        fluidRow(
            optionsBox(
                width = 12,
                column(
                    width = 8,
                    uiOutput(ns("heatmap_class_ui"))
                ),
                column(
                    width = 4,
                    uiOutput(ns("heatmap_response_ui"))
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
}

immunefeatures_correlations <- function(
    input,
    output, 
    session, 
    feature_value_con,
    features_con,
    group_con
){
    
    ns <- session$ns
    
    heatmap_choices_con <- reactive({
        req(features_con())
        features_con() %>% 
            dplyr::select(class, display, feature) %>% 
            dplyr::filter_all(dplyr::all_vars(!is.na(.)))
    })
    
    output$heatmap_class_ui <- renderUI({
        req(heatmap_choices_con)
        selectInput(
            ns("heatmap_class"),
            "Select Variable Class",
            selected = "Immune Cell Proportion - Original",
            choices = get_unique_values_from_column(
                heatmap_choices_con(),
                "class"
            )
        )
    })
    
    output$heatmap_response_ui <- renderUI({
        req(heatmap_choices_con)
        selectInput(
            ns("heatmap_response"),
            "Select Response Variable",
            choices = create_nested_named_list(heatmap_choices_con()),
            selected = "Leukocyte Fraction"
        )
    })
    
    response_display_name <- reactive({
        req(heatmap_choices_con, input$heatmap_response)
        translate_value(
            heatmap_choices_con(),
            input$heatmap_response,
            "feature",
            "display"
        )
    })
    
    feature_choices <- reactive({
        req(heatmap_choices_con, input$heatmap_class)
        translate_values(
            heatmap_choices_con(),
            input$heatmap_class,
            "class",
            "feature"
        )
    })
    
    heatmap_con <- reactive({
        req(
            feature_value_con(),
            heatmap_choices_con(),
            feature_choices(),
            input$heatmap_response
        )
        build_immune_feature_heatmap_con(
            feature_value_con(),
            heatmap_choices_con(),
            feature_choices(),
            input$heatmap_response
        )
    })
    
    heatmap_matrix <- reactive({
        req(heatmap_con(), input$correlation_method)
        build_immune_feature_heatmap_matrix(
            heatmap_con(), 
            input$correlation_method
        )
    })
    
    output$heatmap <- renderPlotly({
        create_heatmap(heatmap_matrix(), "heatplot", scale_colors = T)
    })
    
    
    output$heatmap_group_text <- renderText({
        req(group_con())
        eventdata <- event_data("plotly_click", source =  "heatplot")
        validate(need(eventdata, "Click above heatmap"))
        create_immune_features_heatmap_text(eventdata, group_con())
    })
    
    
    output$scatterPlot <- renderPlotly({
        req(heatmap_con(), response_display_name())
        eventdata <- event_data("plotly_click", source = "heatplot")
        validate(need(eventdata, "Click above heatmap"))
        
        clicked_group <- eventdata$x[[1]]
        clicked_feature <- eventdata$y[[1]]
        
        heatmap_con() %>% 
            build_immune_feature_scatterplot_tbl(clicked_feature) %>% 
            create_scatterplot(
                xlab =  clicked_feature,
                ylab =  response_display_name(),
                title = clicked_group,
                label_col = "label",
                fill_colors = "blue"
            )
    })
}

