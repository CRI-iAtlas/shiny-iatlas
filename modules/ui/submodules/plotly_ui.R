plotly_ui <- function(
    id,
    click_text = "Click plot to see group information.",
    button_text = "Download plot table"
){
    
    ns <- NS(id)
    
    .GlobalEnv$plotBox(
        width = 12,
        "plot" %>% 
            ns() %>% 
            plotly::plotlyOutput() %>%
            shinycssloaders::withSpinner(),
        shiny::conditionalPanel(
            condition =  "output.show_group_text",
            shiny::textOutput(ns("plot_group_text")),
            shiny::h4(click_text),
            ns = ns
        ),
        shiny::downloadButton(ns("download_tbl"), button_text)
    )
}