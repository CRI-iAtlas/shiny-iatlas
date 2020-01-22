overall_cell_proportions_ui <- function(id){
    
    ns <- shiny::NS(id)
    
    .GlobalEnv$sectionBox(
        title = "Overall Cell Proportions",
        .GlobalEnv$messageBox(
            width = 12,
            shiny::includeMarkdown(
                "data/markdown/overall_cell_proportions1.markdown"
            )
        ),
        shiny::fluidRow(
            .GlobalEnv$plotBox(
                width = 12,
                plotly::plotlyOutput(ns("barplot")) %>% 
                    shinycssloaders::withSpinner(),
                shiny::textOutput(ns("barplot_text"))
            )
        ),
        shiny::fluidRow(
            .GlobalEnv$plotBox(
                width = 12,
                .GlobalEnv$messageBox(
                    width = 6,
                    shiny::includeMarkdown(
                        "data/markdown/overall_cell_proportions2.markdown"
                    )
                ),
                column(
                    width = 6,
                    shiny::br(),
                    shiny::fluidRow(
                        plotly::plotlyOutput(ns("scatterplot")) %>%
                            shinycssloaders::withSpinner()
                    )
                )
            )
        )
    )
}