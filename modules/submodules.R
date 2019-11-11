data_table_module_UI <- function(
    id, 
    title = "", 
    message_html = ""
){
    
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
    data_df,
    options = list(pageLength = 10),
    color = F,
    color_column = NULL,
    colors = NULL,
    ...
){
    output$data_table_module <- DT::renderDT({
        dt <- DT::datatable(
            data_df(),
            options = options,
            rownames = FALSE,
            ...
        )
        if(color){
            dt <-  DT::formatStyle(
                dt,
                color_column,
                backgroundColor = DT::styleEqual(colors, colors))
        }
        return(dt)
    })
}
