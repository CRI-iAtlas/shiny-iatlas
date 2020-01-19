clinical_outcomes_server <- function(
    input, 
    output, 
    session, 
    feature_values_con,
    features_con,
    groups_con,
    group_name,
    cohort_colors
){
    ns <- session$ns
    
    # survival plot ----
    survival_tbl <- reactive({

        req(
            features_con(),
            feature_values_con(),
            input$suvivial_time_feature_choice
        )
        
        build_survival_tbl(
            features_con(),
            feature_values_con(),
            input$suvivial_time_feature_choice
        ) 
    })
    
    output$survival_plot <- renderPlot({
        
        shiny::req(
            survival_tbl(),
            cohort_colors(),
            group_name(),
            input$risktable
        )
        
        
        shiny::validate(shiny::need(
            nrow(survival_tbl()) > 0, 
            "Samples with selected variable don't have selected survival feature"
        ))
        
        num_groups <- length(unique(survival_tbl()$group))
        
        shiny::validate(shiny::need(
            num_groups <= 10,
            paste0(
                "Too many sample groups (", num_groups, ") ",
                "for KM plot; choose a continuous variable or select ",
                "different sample groups."
            )
        ))
        
        fit <- survival::survfit(
            survival::Surv(time, status) ~ group, 
            data = survival_tbl()
        )
        
        create_kmplot(
            fit = fit,
            df = survival_tbl(),
            confint = input$confint,
            risktable = input$risktable,
            title = group_name(),
            group_colors = unname(cohort_colors()))
    })
    
    # heatmap plot ----
    
    output$survival_class_opts <- renderUI({
        
        
        req(features_con())
        choices <- features_con() %>% 
            dplyr::select(class_name, class_id) %>% 
            dplyr::distinct() %>% 
            dplyr::collect() %>% 
            tibble::deframe() 
        
        selectInput(
            ns("survival_class_id"),
            "Select or Search for Variables Class",
            choices = choices,
            selected = 12
        )
    })
    
    heatmap_time_feature_id <- reactive({
        req(input$heatmap_time_feature_choice, features_con())
        features_con() %>% 
            dplyr::filter(feature_name == local(input$heatmap_time_feature_choice)) %>% 
            dplyr::pull(feature_id)
    })
    
    heatmap_status_feature_id <- reactive({
        req(input$heatmap_time_feature_choice, features_con())
        
        if (input$heatmap_time_feature_choice == "OS Time") {
            status_feature <- "OS"
        } else if (input$heatmap_time_feature_choice == "PFI Time"){
            status_feature <- "PFI"
        } else {
            stop("input$time_feature_choice is not a valid choice")
        }
        features_con() %>% 
            dplyr::filter(feature_name == status_feature) %>% 
            dplyr::pull(feature_id)
    })
    
    heatmap_feature_ids <- reactive({
        req(features_con(), input$survival_class_id)
        
        features_con() %>% 
            dplyr::filter(class_id == local(as.numeric(input$survival_class_id))) %>% 
            dplyr::arrange(order) %>% 
            dplyr::pull(feature_id)
    })
    
    heatmap_feature_values_con <- reactive({
        req(feature_values_con(), heatmap_feature_ids())
        feature_values_con() %>% 
            dplyr::filter(feature_id %in% local(heatmap_feature_ids())) %>% 
            dplyr::inner_join(features_con(), by = "feature_id") %>% 
            dplyr::select(sample_id, feature_name, value) %>% 
            dplyr::compute()
    })
    
    heatmap_time_feature_con <- reactive({
        req(
            heatmap_time_feature_id(),
            feature_values_con(),
            features_con()
        )
        
        feature_values_con() %>% 
            dplyr::filter(feature_id %in% local(heatmap_time_feature_id())) %>% 
            dplyr::filter(!is.na(value)) %>% 
            dplyr::inner_join(features_con(), by = "feature_id") %>% 
            dplyr::select(sample_id, group, time = value) %>% 
            dplyr::compute()
    })
    
    heatmap_status_feature_con <- reactive({
        req(
            heatmap_status_feature_id(),
            feature_values_con(),
            features_con()
        )
        
        feature_values_con() %>% 
            dplyr::filter(feature_id %in% local(heatmap_status_feature_id())) %>% 
            dplyr::filter(!is.na(value)) %>% 
            dplyr::inner_join(features_con(), by = "feature_id") %>% 
            dplyr::select(sample_id, status = value) %>% 
            dplyr::compute()
    })
    
    
    heatmap_survial_values_con <- reactive({
        req(
            heatmap_time_feature_con(),
            heatmap_status_feature_con()
        )
        
        con <-
            dplyr::inner_join(
                heatmap_time_feature_con(),
                heatmap_status_feature_con(),
                by = "sample_id"
            ) %>%  
            dplyr::select(sample_id, time, status, group) %>% 
            dplyr::compute() 
    })
    
    output$heatmapplot <- plotly::renderPlotly({
        
        shiny::req(
            heatmap_survial_values_con(),
            heatmap_feature_values_con()
        )

        ci_mat <- build_ci_matrix(
            heatmap_feature_values_con(),
            heatmap_survial_values_con()
        )
        
        shiny::validate(shiny::need(
            nrow(ci_mat > 0) & ncol(ci_mat > 0), 
            "No results to display, pick a different group."
        ))

        create_heatmap(ci_mat, "ci")
    })
    
    output$heatmap_group_text <- renderText({
        shiny::req(groups_con)
        eventdata <- plotly::event_data("plotly_click", source = "ci")
        shiny::validate(shiny::need(eventdata, "Click above plot"))
        
        groups_con() %>% 
            dplyr::filter(group == local(unique(dplyr::pull(eventdata, "x")))) %>% 
            dplyr::mutate(text = paste0(name, ": ", characteristics)) %>%
            dplyr::pull(text)
    })
    
    
}


