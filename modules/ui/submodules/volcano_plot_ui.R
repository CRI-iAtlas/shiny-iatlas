volcano_plot_ui <- function(id){
    ns <- NS(id)
    tagList(
        fluidRow(
            plotBox(
                width = 12,
                plotlyOutput(ns("volcano_plot")) %>% 
                    shinycssloaders::withSpinner()
            )
        ),
        fluidRow(
            plotBox(
                width = 12,
                plotlyOutput(ns("violin_plot")) %>%
                    shinycssloaders::withSpinner()
            )
        )
    )
}