clinical_outcomes_heatmap_server <- function(
    input, 
    output, 
    session, 
    sample_tbl,
    group_tbl,
    group_name,
    plot_colors
){
    
    ns <- session$ns
    
    source("functions/clinical_outcomes_heatmap_functions.R", local = T)
    
    output$class_selection_ui <- shiny::renderUI({
        
        shiny::req(feature_named_list())
        shiny::selectInput(
            ns("class_choice_id"),
            "Select or Search for Variables Class",
            choices = create_class_list(),
            selected = get_t_helper_score_class_id()
        )
    })
    
    time_feature_id <- shiny::reactive({
        shiny::req(input$time_feature_choice)
        get_feature_id(input$time_feature_choice)
    })
    
    status_feature_name <- shiny::reactive({
        shiny::req(input$time_feature_choice)
        get_status_feature_name(input$time_feature_choice)
    })
    
    status_feature_id <- shiny::reactive({
        shiny::req(status_feature_name())
        get_feature_id(status_feature_name())

    })
    
    value_tbl <- shiny::reactive({
        shiny::req(
            sample_tbl(),
            input$class_choice_id,
            time_feature_id(),
            status_feature_id()
        )
        build_value_tbl(
            sample_tbl(),
            input$class_choice_id,
            time_feature_id(),
            status_feature_id()
        )
    })
    
    output$heatmap <- plotly::renderPlotly({
        shiny::req(value_tbl())

        mat <- build_hetamap_matrix(value_tbl())
        
        shiny::validate(shiny::need(
            nrow(mat > 0) & ncol(mat > 0), 
            "No results to display, pick a different group."
        ))
        
        create_heatmap(mat, "clinical_outcomes_heatmap")
    })
    
    output$heatmap_group_text <- shiny::renderText({
        shiny::req(group_tbl)
        eventdata <- plotly::event_data("plotly_click", source = "clinical_outcomes_heatmap")
        shiny::validate(shiny::need(eventdata, "Click above plot"))

        group_tbl() %>%
            dplyr::filter(group == local(unique(dplyr::pull(eventdata, "x")))) %>%
            dplyr::mutate(text = paste0(name, ": ", characteristics)) %>%
            dplyr::pull(text)
    })
    
    
}