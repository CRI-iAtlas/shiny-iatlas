immune_feature_correlations_server <- function(
    input,
    output, 
    session, 
    sample_tbl,
    group_tbl,
    feature_named_list
){
    
    ns <- session$ns
    
    source("functions/immune_feature_correlations_functions.R", local = T)
    
    output$class_selection_ui <- shiny::renderUI({
        shiny::selectInput(
            ns("class_choice_id"),
            "Select or Search for Variable Class",
            selected = get_dna_alteration_class_id(),
            choices = create_class_list()
        )
    })
    
    output$response_selection_ui <- shiny::renderUI({
        req(feature_named_list())

        shiny::selectInput(
            ns("response_choice_id"),
            "Select or Search for Response Variable",
            choices = feature_named_list(),
            selected = get_leukocyte_fraction_id()
        )
    })
    
    response_name <- shiny::reactive({
        shiny::req(input$response_choice_id)
        get_feature_name(input$response_choice_id)
    })
    
    value_tbl <- shiny::reactive({
        shiny::req(
            sample_tbl(),
            input$class_choice_id,
            input$response_choice_id
        )
        build_value_tbl(
            sample_tbl(),
            input$class_choice_id,
            input$response_choice_id
        )
    })
    
    heatmap_matrix <- shiny::reactive({
        shiny::req(
            value_tbl(),
            input$correlation_method
        )
        build_heatmap_matrix(
            value_tbl(),
            input$correlation_method
        )
    })
    
    output$heatmap <- plotly::renderPlotly({
        shiny::req(heatmap_matrix())
        create_heatmap(
            heatmap_matrix(), 
            "immune_features_heatmap",
            scale_colors = T
        )
    })
    
    
    output$heatmap_group_text <- shiny::renderText({
        shiny::req(group_tbl())
        eventdata <- plotly::event_data(
            "plotly_click", 
            source = "immune_features_heatmap"
        )
        shiny::validate( shiny::need(eventdata, "Click above heatmap"))
        group_tbl() %>% 
            dplyr::filter(group == local(unique(dplyr::pull(eventdata, "x")))) %>% 
            dplyr::mutate(text = paste0(name, ": ", characteristics)) %>%
            dplyr::pull(text)
    })
    
    
    output$scatterPlot <- plotly::renderPlotly({
        shiny::req(value_tbl(), response_name())
        eventdata <- plotly::event_data(
            "plotly_click", 
            source = "immune_features_heatmap"
        )
        shiny::validate(shiny::need(eventdata, "Click above heatmap"))
        
        clicked_group <- eventdata$x[[1]]
        clicked_feature <- eventdata$y[[1]]
        
        shiny::validate(shiny::need(
            all(
                clicked_feature %in% value_tbl()$feature,
                clicked_group %in% value_tbl()$group
            ),
            "Click above heatmap"
        ))
        
        value_tbl() %>% 
            build_scatterplot_tbl(clicked_feature, clicked_group) %>% 
            create_scatterplot(
                xlab =  clicked_feature,
                ylab =  response_name(),
                title = clicked_group,
                label_col = "label",
                fill_colors = "blue"
            )
    })
}

