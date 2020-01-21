clinical_outcomes_survival_ui <- function(id) {
    ns <- NS(id)
    
    .GlobalEnv$sectionBox(
        title = "Sample Group Survival",
        .GlobalEnv$messageBox(
            width = 12,
            shiny::includeMarkdown(
                "data/markdown/clinical_outcomes_survival.markdown"
            )
        ),
        shiny::fluidRow(
            .GlobalEnv$optionsBox(
                width = 12,
                shiny::column(
                    width = 8,
                    shiny::selectInput(
                        inputId = ns("suvivial_time_feature_choice"),
                        label ="Select or Search for Survival Endpoint",
                        selected = "OS Time",
                        choices = c(
                            "Overall Survival" = "OS Time", 
                            "Progression Free Interval" = "PFI Time"
                        )
                    )
                ),
                shiny::column(
                    width = 2,
                    shiny::checkboxInput(
                        ns("confint"), 
                        "Confidence Intervals", 
                        value = F
                    )
                ),
                shiny::column(
                    width = 2,
                    shiny::checkboxInput(
                        ns("risktable"),
                        "Risk Table",
                        value = T
                    )
                )
            ),
            .GlobalEnv$plotBox(
                width = 12,
                "survival_plot" %>% 
                    ns() %>% 
                    shiny::plotOutput(height = 600) %>%
                    shinycssloaders::withSpinner()
            )
        )
    )
}