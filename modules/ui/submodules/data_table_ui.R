data_table_ui <- function(
    id, 
    title = "", 
    message_html = ""
){
    
    ns <- shiny::NS(id)
    
    sectionBox(
        title = title,
        .GlobalEnv$messageBox(width = 12, message_html),
        shiny::fluidRow(
            .GlobalEnv$tableBox(
                width = 12,
                shiny::div(style = "overflow-x: scroll",
                    DT::dataTableOutput(ns("data_table_module")) %>%
                        shinycssloaders::withSpinner()
                )
            )
        )
    )
}