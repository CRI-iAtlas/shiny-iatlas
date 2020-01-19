volcano_plot_ui <- function(id){
    ns <- shiny::NS(id)
    shiny::tagList(
        fluidRow(
            .GlobalEnv$plotBox(
                width = 12,
                "volcano_plot" %>% 
                    ns() %>% 
                    plotly::plotlyOutput() %>% 
                    shinycssloaders::withSpinner()
            )
        ),
        shiny::fluidRow(
            .GlobalEnv$plotBox(
                width = 12,
                "violin_plot" %>% 
                    ns() %>% 
                    plotly::plotlyOutput() %>%
                    shinycssloaders::withSpinner()
            )
        )
    )
}