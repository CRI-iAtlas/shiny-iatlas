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
    cohort_sample_tbl,
    cohort_group_con,
    cohort_group_name,
    cohort_colors
){
    
    distributions_feature_value_con <- reactive({
        con <- 
            create_conection("features_to_samples") %>% 
            # dplyr::filter(sample_id %in% 1:100)  %>% 
            dplyr::filter(sample_id %in% local(cohort_sample_tbl()$sample_id)) %>%
            dplyr::select(sample_id, feature_id, value)
    })
    

    distributions_feature_con <- reactive({
        con <- 
            create_conection("features_to_samples") %>% 
            dplyr::filter(sample_id %in% 1:100)  %>% 
            # dplyr::filter(sample_id %in% cohort_sample_tbl$sample_id) %>% 
            dplyr::select(id = feature_id) %>% 
            dplyr::distinct() %>% 
            dplyr::inner_join(create_conection("features"),  by = "id") %>% 
            dplyr::inner_join(create_conection("classes"),  by = c("class_id" = "id")) %>% 
            dplyr::select(DISPLAY = display, INTERNAL = id, CLASS = name.y)
    })
    
    callModule(
        distributions_plot_module,
        "dist",
        plot_source_name           = "immunefeatures_dist_plot",
        feature_value_con          = distributions_feature_value_con,
        feature_metadata_con       = distributions_feature_con,
        cohort_sample_tbl          = cohort_sample_tbl,
        groups_con                 = cohort_group_con,
        group_display_choice       = cohort_group_name,
        plot_colors                = cohort_colors,
        variable_selection_default = 36,
        key_col                    = "label"
    )
    
    ### immune features correlations
    
    callModule(
        immunefeatures_correlations,
        "immunefeatures_correlations",
        sample_tbl = cohort_sample_tbl,
        group_con  = cohort_group_con
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
    sample_tbl,
    group_con
){
    
    ns <- session$ns
    
    features_con <- reactive({
        con <- 
            dplyr::inner_join(
                create_conection("features"),
                create_conection("classes"),
                by = c("class_id" = "id")
            ) %>% 
            dplyr::arrange(name.y, order) %>% 
            dplyr::select(class_name = name.y, class_id, feature_name = display, feature_id = id, order)
    })
    
    output$heatmap_class_selection_ui <- renderUI({
        req(features_con())
        choices <- features_con() %>% 
            dplyr::select(class_name, class_id) %>% 
            dplyr::distinct() %>% 
            dplyr::as_tibble() %>% 
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
            dplyr::filter(feature_id == local(
                as.numeric(input$heatmap_response_choice_id))
            ) %>% 
            dplyr::pull(feature_name)
    })
    
    feature_ids <- reactive({
        req(features_con(), input$heatmap_class_choice_id)
        con <- features_con() %>% 
            dplyr::filter(class_id == local(
                as.numeric(input$heatmap_class_choice_id))
            ) %>% 
            dplyr::arrange(order) %>% 
            dplyr::pull(feature_id)
        
    })
    
    heatmap_tbl <- reactive({
        req(
            feature_ids(),
            input$heatmap_response_choice_id,
            sample_tbl()
        )
        
        build_immune_feature_heatmap_tbl(
            as.numeric(feature_ids()),
            as.numeric(input$heatmap_response_choice_id),
            sample_tbl()
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
        
        heatmap_tbl() %>% 
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

