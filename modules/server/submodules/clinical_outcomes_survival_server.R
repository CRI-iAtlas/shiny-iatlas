clinical_outcomes_survival_server <- function(
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
    
    survival_tbl <- shiny::reactive({

        shiny::req(
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

    output$survival_plot <- shiny::renderPlot({

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
}