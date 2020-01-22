clinical_outcomes_survival_server <- function(
    input, 
    output, 
    session, 
    sample_tbl,
    group_tbl,
    group_name,
    plot_colors
){
    
    ns <- session$ns
    
    source("functions/clinical_outcomes_survival_functions.R")
    
    status_feature_name <- reactive({
        shiny::req(input$time_feature_choice)
        get_status_feature_name(input$time_feature_choice)
    })
    
    value_tbl <- shiny::reactive({
        shiny::req(
            sample_tbl(),
            input$time_feature_choice, 
            status_feature_name()
        )
        build_value_tbl(
            sample_tbl(),
            input$time_feature_choice, 
            status_feature_name()
        )
    })
    

    output$survival_plot <- shiny::renderPlot({

        shiny::req(
            value_tbl(),
            plot_colors(),
            group_name(),
            input$risktable
        )

        shiny::validate(shiny::need(
            nrow(value_tbl()) > 0,
            "Samples with selected variable don't have selected survival feature"
        ))

        num_groups <- length(unique(value_tbl()$group))

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
            data = value_tbl()
        )

        .GlobalEnv$create_kmplot(
            fit = fit,
            df = value_tbl(),
            confint = input$confint,
            risktable = input$risktable,
            title = group_name(),
            group_colors = unname(plot_colors()))
    })
}