immune_feature_correlations_ui <- function(id) {
    
    ns <- shiny::NS(id)
    
    sectionBox(
        title = "Correlations",
        .GlobalEnv$messageBox(
            width = 12,
            shiny::includeMarkdown(
                "data/markdown/immune_features_correlations.markdown"
            )
        ),
        shiny::fluidRow(
            .GlobalEnv$optionsBox(
                width = 12,
                shiny::column(
                    width = 8,
                    shiny::uiOutput(ns("class_selection_ui"))
                ),
                shiny::column(
                    width = 4,
                    shiny::uiOutput(ns("response_selection_ui"))
                ),
                shiny::column(
                    width = 4,
                    shiny::selectInput(
                        ns("correlation_method"),
                        "Select or Search for Correlation Method",
                        choices = c(
                            "Pearson"  = "pearson",
                            "Spearman" = "spearman",
                            "Kendall"  = "kendall"
                        ),
                        selected = "spearman"
                    )
                )
            )
        ),
        shiny::fluidRow(
            .GlobalEnv$plotBox(
                width = 12,
                shiny::fluidRow(
                    "heatmap" %>% 
                        ns() %>% 
                        plotly::plotlyOutput() %>% 
                        shinycssloaders::withSpinner(),
                    shiny::textOutput(ns("heatmap_group_text"))
                )
            )
        ),
        shiny::fluidRow(
            .GlobalEnv$plotBox(
                width = 12,
                "scatterPlot" %>% 
                    ns() %>% 
                    plotly::plotlyOutput() %>%
                    shinycssloaders::withSpinner()
            )
        )
    )
}