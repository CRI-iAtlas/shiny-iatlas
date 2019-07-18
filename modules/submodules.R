immunomodulator_table_UI <- function(id){
    
    ns <- NS(id)
    message <- stringr::str_c(
        "The table shows annotations of the immumodulators, and source.",
        "Use the Search box in the upper right to find an immumodulator of",
        "interest.",
        sep = " "
    )
    
    sectionBox(
        title = "Immunomodulator Annotations",
        messageBox(
            width = 12,
            p(message)  
        ),
        fluidRow(
            tableBox(
                width = 12,
                div(style = "overflow-x: scroll",
                    DT::dataTableOutput(ns("im_annotations_table")) %>%
                        shinycssloaders::withSpinner()
                )
            )
        )
    )
}

immunomodulator_table <- function(
    input, 
    output, 
    session
){
    output$im_annotations_table <- DT::renderDT({
        
        panimmune_data$im_direct_relationships %>% 
            dplyr::select(-X10, -Notes) %>% 
            DT::datatable(
                options = list(pageLength = 10),
                rownames = FALSE
            )
    })
}

