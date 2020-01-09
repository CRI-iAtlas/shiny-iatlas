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
    feature_values_con,
    features_con,
    group_con,
    group_name,
    cohort_colors
){
    
    distributions_feature_con <- reactive({
        features_con() %>%
            dplyr::select(DISPLAY = feature_name, INTERNAL = feature_id, CLASS = class_name)
    })

    callModule(
        distributions_plot_module,
        "dist",
        plot_source_name           = "immunefeatures_dist_plot",
        feature_values_con         = feature_values_con,
        feature_metadata_con       = distributions_feature_con,
        groups_con                 = group_con,
        group_display_choice       = group_name,
        plot_colors                = cohort_colors,
        variable_selection_default = 36,
        key_col                    = "label"
    )
    
    ### immune features correlations
    
    callModule(
        immunefeatures_correlations,
        "immunefeatures_correlations",
        feature_values_con = feature_values_con, 
        features_con = features_con, 
        group_con  = group_con
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
                    uiOutput(ns("heatmap_class_selection_ui"))
                ),
                column(
                    width = 4,
                    uiOutput(ns("heatmap_response_selection_ui"))
                ),
                column(
                    width = 4,
                    selectInput(
                        ns("correlation_method"),
                        "Select Correlation Method",
                        choices = c(
                            "Pearson"  = "pearson",
                            "Spearman" = "spearman",
                            "Kendall"  = "kendall"
                        ),
                        selected = "spearman"
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
    feature_values_con,
    features_con,
    group_con
){
    
    ns <- session$ns

    output$heatmap_class_selection_ui <- renderUI({
        req(features_con())
        choices <- features_con() %>% 
            dplyr::select(class_name, class_id) %>% 
            dplyr::distinct() %>% 
            dplyr::collect() %>% 
            tibble::deframe()
        
        selectInput(
            ns("heatmap_class_choice_id"),
            "Select Variable Class",
            selected = 6,
            choices = choices
        )
    })
    
    output$heatmap_response_selection_ui <- renderUI({
        req(features_con())
        choices <- features_con() %>% 
            dplyr::select(
                class = class_name, 
                display = feature_name, 
                feature = feature_id
            ) %>% 
            dplyr::collect() %>% 
            create_nested_named_list()
        
        selectInput(
            ns("heatmap_response_choice_id"),
            "Select Response Variable",
            choices = choices,
            selected = 36
        )
    })
    
    response_display_name <- reactive({
        req(features_con(), input$heatmap_response_choice_id)
        features_con() %>% 
            dplyr::filter(
                feature_id == local(as.numeric(input$heatmap_response_choice_id))
            ) %>% 
            dplyr::pull(feature_name)
    })
    
    feature_ids <- reactive({
        req(features_con(), input$heatmap_class_choice_id)
        con <- features_con() %>% 
            dplyr::filter(
                class_id == local(as.numeric(input$heatmap_class_choice_id))
            ) %>% 
            dplyr::arrange(order) %>% 
            dplyr::pull(feature_id)
        
    })
    
    heatmap_response_con <- reactive({
        req(
            feature_values_con(),
            features_con(),
            input$heatmap_response_choice_id
        )
        
        build_immune_feature_heatmap_response_con(
            feature_values_con(),
            features_con(),
            as.numeric(input$heatmap_response_choice_id)
        )
    })
    
    heatmap_feature_con <- reactive({
        req(
            feature_values_con(),
            features_con(),
            feature_ids()
        )
        
        build_immune_feature_heatmap_feature_con(
            feature_values_con(),
            features_con(),
            as.numeric(feature_ids())
        )
    })
    
    heatmap_tbl <- reactive({
        req(
            heatmap_response_con(),
            heatmap_feature_con()
        )
        
        build_immune_feature_heatmap_tbl(
            heatmap_response_con(),
            heatmap_feature_con()
        )
    })
    
    heatmap_matrix <- reactive({
        req(heatmap_tbl(), input$correlation_method)
        build_immune_feature_heatmap_matrix(
            heatmap_tbl(), 
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
        group_con() %>% 
            dplyr::filter(group == local(unique(dplyr::pull(eventdata, "x")))) %>% 
            dplyr::mutate(text = paste0(name, ": ", characteristics)) %>%
            dplyr::pull(text)
    })
    
    
    output$scatterPlot <- renderPlotly({
        req(heatmap_tbl(), response_display_name())
        eventdata <- event_data("plotly_click", source = "heatplot")
        validate(need(eventdata, "Click above heatmap"))
        
        clicked_group <- eventdata$x[[1]]
        clicked_feature <- eventdata$y[[1]]
        
        heatmap
        
        validate(need(
            all(
                clicked_feature %in% heatmap_tbl()$feature,
                clicked_group %in% heatmap_tbl()$group
            ),
            "Click above heatmap"
        ))
        
        heatmap_tbl() %>% 
            build_immune_feature_scatterplot_tbl(clicked_feature, clicked_group) %>% 
            create_scatterplot(
                xlab =  clicked_feature,
                ylab =  response_display_name(),
                title = clicked_group,
                label_col = "label",
                fill_colors = "blue"
            )
    })
}

