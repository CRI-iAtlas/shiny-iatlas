immune_features_server <- function(
    input,
    output, 
    session,
    sample_tbl,
    group_tbl,
    group_name,
    feature_named_list,
    plot_colors
){
    
    source(
        "modules/server/submodules/immune_feature_distributions_server.R",
        local = T
    )
    
    shiny::callModule(
        immune_feature_distributions_server,
        "immune_feature_distributions",
        sample_tbl,
        group_tbl,
        group_name,
        feature_named_list,
        plot_colors
    )
    

    shiny::callModule(
        immune_features_correlations_server,
        "immunefeatures_correlations",
        feature_values_con = feature_values_con, 
        features_con = features_con, 
        group_con  = group_con
    )

}



immune_features_correlations_server <- function(
    input,
    output, 
    session, 
    feature_values_con,
    features_con,
    group_con
){
    
    ns <- session$ns

    output$heatmap_class_selection_ui <- shiny::renderUI({
        shiny::req(features_con())
        choices <- features_con() %>% 
            dplyr::select(class_name, class_id) %>% 
            dplyr::distinct() %>% 
            dplyr::collect() %>% 
            tibble::deframe()
        
        shiny::selectInput(
            ns("heatmap_class_choice_id"),
            "Select or Search for Variable Class",
            selected = 6,
            choices = choices
        )
    })
    
    output$heatmap_response_selection_ui <- shiny::renderUI({
        shiny::req(features_con())
        choices <- features_con() %>% 
            dplyr::select(
                class = class_name, 
                display = feature_name, 
                feature = feature_id
            ) %>% 
            dplyr::collect() %>% 
            create_nested_named_list()
        
        shiny::selectInput(
            ns("heatmap_response_choice_id"),
            "Select or Search for Response Variable",
            choices = choices,
            selected = 36
        )
    })
    
    response_display_name <- shiny::reactive({
        shiny::req(features_con(), input$heatmap_response_choice_id)
        features_con() %>% 
            dplyr::filter(
                feature_id == local(as.numeric(input$heatmap_response_choice_id))
            ) %>% 
            dplyr::pull(feature_name)
    })
    
    feature_ids <- shiny::reactive({
        shiny::req(features_con(), input$heatmap_class_choice_id)
        con <- features_con() %>% 
            dplyr::filter(
                class_id == local(as.numeric(input$heatmap_class_choice_id))
            ) %>% 
            dplyr::arrange(order) %>% 
            dplyr::pull(feature_id)
        
    })
    
    heatmap_response_con <- shiny::reactive({
        shiny::req(
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
    
    heatmap_feature_con <- shiny::reactive({
        shiny::req(
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
    
    heatmap_tbl <- shiny::reactive({
        shiny::req(
            heatmap_response_con(),
            heatmap_feature_con()
        )
        
        build_immune_feature_heatmap_tbl(
            heatmap_response_con(),
            heatmap_feature_con()
        )
    })
    
    heatmap_matrix <- shiny::reactive({
        shiny::req(heatmap_tbl(), input$correlation_method)
        build_immune_feature_heatmap_matrix(
            heatmap_tbl(), 
            input$correlation_method
        )
    })
    
    output$heatmap <- plotly::renderPlotly({
        create_heatmap(heatmap_matrix(), "heatplot", scale_colors = T)
    })
    
    
    output$heatmap_group_text <- shiny::renderText({
        shiny::req(group_con())
        eventdata <- plotly::event_data("plotly_click", source =  "heatplot")
        shiny::validate( shiny::need(eventdata, "Click above heatmap"))
        group_con() %>% 
            dplyr::filter(group == local(unique(dplyr::pull(eventdata, "x")))) %>% 
            dplyr::mutate(text = paste0(name, ": ", characteristics)) %>%
            dplyr::pull(text)
    })
    
    
    output$scatterPlot <- plotly::renderPlotly({
        shiny::req(heatmap_tbl(), response_display_name())
        eventdata <- plotly::event_data("plotly_click", source = "heatplot")
        shiny::validate(shiny::need(eventdata, "Click above heatmap"))
        
        clicked_group <- eventdata$x[[1]]
        clicked_feature <- eventdata$y[[1]]
        
        heatmap
        
        shiny::validate(shiny::need(
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

