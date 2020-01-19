immune_features_ui <- function(id) {
    
    ns <- NS(id)
    
    source("modules/ui/submodules/distribution_plot_ui.R", local = T)
    
    tagList(
        titleBox("iAtlas Explorer — Immune Feature Trends"),
        textBox(
            width = 12,
            p(stringr::str_c(
                "This module allows you to see how immune readouts vary",
                "across your groups, and how they relate to one another."
            ))  
        ),
        distributions_plot_ui(
            ns("dist"),
            message_html = includeMarkdown("data/markdown/immune_features_dist.markdown"),
        ),
        immune_features_correlations_ui(ns("immunefeatures_correlations"))
    )
}

immune_features_correlations_ui <- function(id) {
    ns <- NS(id)
    
    tagList()
    
    sectionBox(
        title = "Correlations",
        messageBox(
            width = 12,
            includeMarkdown("data/markdown/immune_features_correlations.markdown")
        ),
        fluidRow(
            optionsBox(
                width = 12,
                column(
                    width = 8,
                    uiOutput(ns("heatmap_class_selection_ui"))
                ),
                column(
                    width = 4,
                    uiOutput(ns("heatmap_response_selection_ui"))
                ),
                column(
                    width = 4,
                    selectInput(
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
        fluidRow(
            plotBox(
                width = 12,
                fluidRow(
                    plotly::plotlyOutput(ns("heatmap")) %>% 
                        shinycssloaders::withSpinner(),
                    p(),
                    textOutput(ns("heatmap_group_text"))
                )
            )
        ),
        fluidRow(
            plotBox(
                width = 12,
                plotly::plotlyOutput(ns("scatterPlot")) %>%
                    shinycssloaders::withSpinner()
            )
        )
    )
}