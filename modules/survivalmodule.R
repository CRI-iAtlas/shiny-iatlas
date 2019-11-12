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
                    width = 4,
                    uiOutput(ns("survplot_opts")),
                    
                    selectInput(
                        ns("timevar"),
                        "Survival Endpoint",
                        c("Overall Survival" = "OS_time", "Progression Free Interval" = "PFI_time_1"),
                        selected = "OS_time"
                    ),
                    
                    sliderInput(
                        ns("divk"),
                        "Value Range Divisions",
                        min = 2,
                        max = 10,
                        value = 2
                    ),
                    
                    checkboxInput(ns("confint"), "Confidence Intervals", value = F),
                    checkboxInput(ns("risktable"), "Risk Table", value = T)
                ),
                
                # ** Survival Kaplan-Meier plot ----
                plotBox(
                    width = 8,
                    plotOutput(ns("survPlot"), height = 600) %>%
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
                    width = 4,
                    radioButtons(
                        ns("survival_type"),
                        "Survival Endpoint",
                        c("Progression Free Interval" = "PFI",
                          "Overall Survival" = "OS"
                        ),
                        selected = "PFI"
                    ),
                    uiOutput(ns("survival_class_opts"))
                ),
                plotBox(
                    width = 8,
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
    features_named_list,
    group_display_choice,
    group_internal_choice,
    feature_values_con,
    groups_list,
    groups_con,
    features_con,
    plot_colors
){
    ns <- session$ns
    
    output$survplot_opts <- renderUI({
        req(
            features_named_list(), 
            group_display_choice(), 
            group_internal_choice()
        )

        choices <- 
            group_internal_choice() %>% 
            purrr::set_names(group_display_choice()) %>% 
            list() %>% 
            purrr::set_names("Current Sample Groups") %>% 
            c(features_named_list())

        selectInput(
            ns("var1_surv"),
            "Variable",
            choices,
            selected = group_internal_choice()
        )
    })
    
    output$survPlot <- renderPlot({
        req(
            feature_values_con(), 
            groups_list(),
            input$var1_surv,
            input$timevar,
            input$divk,
            groups_con(),
            features_con(),
            plot_colors()
        )
        
        survival_tbl <- build_survival_tbl(
            feature_values_con(),
            groups_list(),
            input$var1_surv,
            input$timevar,
            input$divk
        )
        
        validate(need(
            nrow(survival_tbl) > 0, 
            "Samples with selected variable don't have selected survival feature"
        ))
        
        num_groups <- length(unique(survival_tbl$group))
        
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
            data = survival_tbl
        )
        
        if (input$var1_surv %in% groups_list()){
            group_colors <- purrr::set_names(
                plot_colors(), 
                stringr::str_c("group=", names(plot_colors()))
            )
            title <- groups_con() %>% 
                translate_value(
                    input$var1_surv,
                    "parent_group",
                    "parent_group_display"
                ) %>% 
                unique()
        } else {
            group_colors <- viridisLite::viridis(input$divk)
            title <- features_con() %>% 
                translate_value(
                    input$var1_surv,
                    "feature",
                    "display"
                ) %>% 
                unique()
        }
        
        create_kmplot(
            fit = fit,
            df = survival_tbl,
            confint = input$confint,
            risktable = input$risktable,
            title = title,
            group_colors = unname(group_colors))
    })
    
    output$survival_class_opts <- renderUI({
        req(features_named_list())
        
        selectInput(
            ns("survival_class"),
            "Select Variables Class (rows)",
            choices = names(features_named_list()),
            selected = "T Helper Cell Score"
        )
    })
    
    output$heatmapplot <- renderPlotly({
        
        req(
            feature_values_con(),
            features_con(),
            input$survival_type,
            input$survival_class
        )
        
        
        if(input$survival_type == "PFI"){
            time_feature <- "OS_time"
            status_feature <- "OS"
        } else{
            time_feature <- "PFI_time_1"
            status_feature <- "PFI_1"
        }
        
        features <- features_con() %>% 
            dplyr::filter(class == local(input$survival_class)) %>% 
            dplyr::arrange(order) %>% 
            dplyr::pull(feature)
        
        
        ci_mat <- build_ci_matrix(
            feature_values_con(),
            features_con(),
            features,
            time_feature,
            status_feature
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
            dplyr::mutate(text = paste0(group_name, ": ", characteristics)) %>%
            dplyr::pull(text)
    })
    
    
}


