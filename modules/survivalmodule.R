# UI ----

survival_UI <- function(id) {
    ns <- NS(id)
    
    tagList(
        titleBox("iAtlas Explorer — Clinical Outcomes"),
        textBox(
            width = 12,
            p("Plot survival curves based on immune characteristics and identify variables associated with outcome.")  
        ),
        
        # Survival comparison section ----
        sectionBox(
            title = "Sample Group Survival",
            messageBox(
                width = 12,
                p("Select the variable, and outcome in terms of either overall survival (OS) or progression free interval (PFI) endpoints to generate a Kaplan-Meier plot. For a continuous (numeric) variable, the slider can be used to specify how the range of values of that variable is split.  Selecting 2 splits the values by the middle of the range, 3 splits the range into three even intervals and so on."),
                p("For immune subtypes Figure 3A can be generated (OS), and Figure S3A for (PFI).")
            ),
            fluidRow(
                optionsBox(
                    width = 12,
                    column(
                        width = 8,
                        selectInput(
                            ns("suvivial_time_feature_choice"),
                            "Select or Search for Survival Endpoint",
                            c("Overall Survival" = "OS Time", "Progression Free Interval" = "PFI Time"),
                            selected = "OS Time"
                        )
                    ),
                    column(
                        width = 2,
                        checkboxInput(ns("confint"), "Confidence Intervals", value = F)
                    ),
                    column(
                        width = 2,
                        checkboxInput(ns("risktable"), "Risk Table", value = T)
                    )
                ),

                # ** Survival Kaplan-Meier plot ----
                plotBox(
                    width = 12,
                    plotOutput(ns("survival_plot"), height = 600) %>%
                        shinycssloaders::withSpinner()
                )
            )
        ),
        
        # Survival comparison section ----
        sectionBox(
            title = "Concordance Index",
            messageBox(
                width = 12,
                p("Here, you can explore which variables are associated with improved or diminished survival within your sample groups. Select a variable class, and you will get a heatmap, with one row for each variable in that class. For a given variable (row) and sample group (column) red denotes decreased survival, and blue increased survival as the variable is increased."),
                p("Manuscript context:  Selecting variable class “Core Expression Signature”, you can generate Figure 3B. Figures 3C, and Figures S3B, S3C, and S3C can also be generated with different selection options.")
            ),
            fluidRow(
                optionsBox(
                    width = 12,
                    column(
                        width = 6,
                        selectInput(
                            ns("heatmap_time_feature_choice"),
                            "Select or Search for Survival Endpoint",
                            c("Overall Survival" = "OS Time", "Progression Free Interval" = "PFI Time"),
                            selected = "OS Time"
                        ),
                    ),
                    column(
                        width = 6,
                        uiOutput(ns("survival_class_opts"))
                    )
                ),
                plotBox(
                    width = 12,
                    fluidRow(
                        plotlyOutput(ns("heatmapplot"), height = 600) %>%
                            shinycssloaders::withSpinner(),
                        p(),
                        textOutput(ns("heatmap_group_text"))
                    )
                )
            )
        )
    )
}

# Server ----
survival <- function(
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
        
        req(
            survival_tbl(),
            cohort_colors(),
            group_name(),
            input$risktable
        )
        
        
        validate(need(
            nrow(survival_tbl()) > 0, 
            "Samples with selected variable don't have selected survival feature"
        ))
        
        num_groups <- length(unique(survival_tbl()$group))
        
        validate(need(
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
    
    output$heatmapplot <- renderPlotly({
        
        req(
            heatmap_survial_values_con(),
            heatmap_feature_values_con()
        )

        ci_mat <- build_ci_matrix(
            heatmap_feature_values_con(),
            heatmap_survial_values_con()
        )
        
        validate(need(
            nrow(ci_mat > 0) & ncol(ci_mat > 0), 
            "No results to display, pick a different group."
        ))

        create_heatmap(ci_mat, "ci")
    })
    
    output$heatmap_group_text <- renderText({
        req(groups_con)
        eventdata <- event_data("plotly_click", source = "ci")
        validate(need(eventdata, "Click above plot"))
        
        groups_con() %>% 
            dplyr::filter(group == local(unique(dplyr::pull(eventdata, "x")))) %>% 
            dplyr::mutate(text = paste0(name, ": ", characteristics)) %>%
            dplyr::pull(text)
    })
    
    
}


