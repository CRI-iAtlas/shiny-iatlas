clinical_outcomes_heatmap_ui <- function(id) {
    ns <- NS(id)
    
    .GlobalEnv$sectionBox(
        title = "Concordance Index",
        .GlobalEnv$messageBox(
            width = 12,
            shiny::includeMarkdown("data/markdown/clinical_outcomes_heatmap.markdown")
        ),
        fluidRow(
            .GlobalEnv$optionsBox(
                width = 12,
                shiny::column(
                    width = 6,
                    shiny::selectInput(
                        inputId = ns("time_feature_choice"),
                        label = "Select or Search for Survival Endpoint",
                        selected = "OS Time",
                        choices = c(
                            "Overall Survival" = "OS Time", 
                            "Progression Free Interval" = "PFI Time"
                        ),
                    ),
                ),
                shiny::column(
                    width = 6,
                    shiny::uiOutput(ns("survival_class_opts"))
                )
            ),
            .GlobalEnv$plotBox(
                width = 12,
                shiny::fluidRow(
                    "heatmapplot" %>% 
                        ns() %>% 
                        plotly::plotlyOutput(height = 600) %>%
                        shinycssloaders::withSpinner(),
                    shiny::textOutput(ns("heatmap_group_text"))
                )
            )
        )
    )
}