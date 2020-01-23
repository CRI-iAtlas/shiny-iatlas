immune_features_ui <- function(id) {
    
    ns <- shiny::NS(id)
    
    source(
        "modules/ui/submodules/immune_feature_distributions_ui.R",
        local = T
    )
    
    shiny::tagList(
        .GlobalEnv$titleBox("iAtlas Explorer — Immune Feature Trends"),
        .GlobalEnv$textBox(
            width = 12,
            shiny::p(stringr::str_c(
                "This module allows you to see how immune readouts vary ",
                "across your groups, and how they relate to one another."
            ))  
        ),
        immune_feature_distributions_ui(ns("immune_feature_distributions"))
        # immune_features_correlations_ui(ns("immunefeatures_correlations"))
    )
}

immune_features_correlations_ui <- function(id) {
    
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
                    shiny::uiOutput(ns("heatmap_class_selection_ui"))
                ),
                shiny::column(
                    width = 4,
                    shiny::uiOutput(ns("heatmap_response_selection_ui"))
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