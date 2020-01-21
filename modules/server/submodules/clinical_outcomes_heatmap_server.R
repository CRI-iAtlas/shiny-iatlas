clinical_outcomes_heatmap_server <- function(
    input, 
    output, 
    session, 
    cohort_sample_tbl,
    feature_values_con,
    features_con,
    groups_con,
    group_name,
    cohort_colors
){
    
    ns <- session$ns
    
    source("functions/clinical_outcomes_heatmap_functions.R", local = T)
    
    output$survival_class_opts <- shiny::renderUI({
        
        shiny::req(features_con())
        choices <- features_con() %>% 
            dplyr::select(class_name, class_id) %>% 
            dplyr::distinct() %>% 
            dplyr::collect() %>% 
            tibble::deframe() 
        
        shiny::selectInput(
            ns("survival_class_id"),
            "Select or Search for Variables Class",
            choices = choices,
            selected = 12
        )
    })
    
    time_feature_id <- shiny::reactive({
        shiny::req(features_con(), input$time_feature_choice)
        get_time_feature_id(features_con(), input$time_feature_choice)
    })
    
    status_feature_id <- shiny::reactive({
        shiny::req(features_con(), input$time_feature_choice)
        get_status_feature_id(features_con(), input$time_feature_choice)

    })
    
    feature_ids <- shiny::reactive({
        shiny::req(features_con(), input$survival_class_id)
        get_feature_ids(features_con(), input$survival_class_id)
    })
    
    selected_feature_values_con <- shiny::reactive({
        shiny::req(
            feature_values_con(), 
            features_con(), 
            feature_ids()
        )
        build_feature_values_con(
            feature_values_con(), 
            features_con(), 
            feature_ids(),
            sample_tbl()
        )

    })
    
    survial_values_con <- shiny::reactive({
        req(
            feature_values_con(),
            features_con(),
            status_feature_id(),
            time_feature_id()
        )
        build_survival_values_con(
            feature_values_con(),
            features_con(),
            status_feature_id(),
            time_feature_id()
        )

    })
    
    output$heatmapplot <- plotly::renderPlotly({
        
        shiny::req(
            survial_values_con(),
            selected_feature_values_con()
        )
        
        ci_mat <- build_ci_matrix(
            survial_values_con(),
            selected_feature_values_con()
        )
        
        shiny::validate(shiny::need(
            nrow(ci_mat > 0) & ncol(ci_mat > 0), 
            "No results to display, pick a different group."
        ))
        
        create_heatmap(ci_mat, "ci")
    })
    
    output$heatmap_group_text <- shiny::renderText({
        shiny::req(groups_con)
        eventdata <- plotly::event_data("plotly_click", source = "ci")
        shiny::validate(shiny::need(eventdata, "Click above plot"))
        
        groups_con() %>% 
            dplyr::filter(group == local(unique(dplyr::pull(eventdata, "x")))) %>% 
            dplyr::mutate(text = paste0(name, ": ", characteristics)) %>%
            dplyr::pull(text)
    })
    
    
}