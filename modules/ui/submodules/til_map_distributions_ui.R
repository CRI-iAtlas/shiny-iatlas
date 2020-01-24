til_map_distributions_ui <- function(id) {
    
    ns <- shiny::NS(id)
    
    .GlobalEnv$sectionBox(
        title = "Distributions",
        .GlobalEnv$messageBox(
            width = 12, 
            shiny::includeMarkdown(
                "data/markdown/immune_features_dist.markdown"
            ),
        ),
        shiny::fluidRow(
            .GlobalEnv$optionsBox(
                width = 12,
                shiny::column(
                    width = 4,
                    shiny::uiOutput(ns("selection_ui"))
                ),
                shiny::column(
                    width = 4,
                    shiny::selectInput(
                        ns("plot_type"),
                        "Select or Search for Plot Type",
                        choices = c("Violin", "Box")
                    )
                ),
                shiny::column(
                    width = 4,
                    shiny::selectInput(
                        ns("scale_method"), 
                        "Select or Search for variable scaling", 
                        selected = "None",
                        choices = c(
                            "None", 
                            "Log2", 
                            "Log2 + 1",
                            "Log10",
                            "Log10 + 1"
                        ),
                    )
                )
            )
        ),
        .GlobalEnv$plotBox(
            width = 12,
            "distplot" %>% 
                ns() %>% 
                plotly::plotlyOutput() %>%
                shinycssloaders::withSpinner(),
            shiny::textOutput(ns("distplot_group_text")),
            shiny::h4("Click plot to see group information."),
            shiny::downloadButton(
                ns("download_distplot_tbl"), 
                "Download plot table"
            )
        ),
        .GlobalEnv$plotBox(
            width = 12,
            "histplot" %>% 
                ns() %>% 
                plotly::plotlyOutput() %>%
                shinycssloaders::withSpinner(),
            shiny::downloadButton(
                ns("download_histtplot_tbl"), 
                "Download plot table"
            )
        )
    )

}