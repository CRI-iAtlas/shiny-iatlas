# data_table_module ----

data_table_module_UI <- function(id, title = "", message_html = ""){
    
    ns <- NS(id)
    
    sectionBox(
        title = title,
        messageBox(width = 12, message_html),
        fluidRow(
            tableBox(
                width = 12,
                div(style = "overflow-x: scroll",
                    DT::dataTableOutput(ns("data_table_module")) %>%
                        shinycssloaders::withSpinner()
                )
            )
        )
    )
}

data_table_module <- function(
    input, 
    output, 
    session,
    df,
    ...
){
    output$data_table_module <- DT::renderDT({
        DT::datatable(
            df,
            options = list(pageLength = 10),
            rownames = FALSE,
            ...
        )
    })
}
