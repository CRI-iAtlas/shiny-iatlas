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

io_targets_table_UI <- function(id){
    
    ns <- NS(id)

    sectionBox(
        title = "IO Target Annotations",
        messageBox(
            width = 12,
            p("The table shows annotations of the IO Targets, with columns as described above and description based on public resources such as NCBI. Use the Search box in the upper right to find an IO target of interest."),
            p("The last column provides a direct link to target information on the IO Landscape resource such as number of target agents under active development, and development stage.")  
        ),
        fluidRow(
            tableBox(
                width = 12,
                div(style = "overflow-x: scroll",
                    DT::dataTableOutput(ns("io_target_annotations_table")) %>%
                        shinycssloaders::withSpinner()
                )
            )
        )
    )
}

io_targets_table <- function(
    input, 
    output, 
    session
){
    output$io_target_annotations_table <- DT::renderDT({
        
        panimmune_data$io_target_annotations %>% 
            dplyr::mutate(LinkText=.$IO_target_URL %>% stringr::str_split(";") %>% purrr::map(last) %>% purrr::flatten_chr()) %>%
            dplyr::mutate(`Link to IO Landscape`=paste("<a href=\"",IO_target_URL,"\">",LinkText,"</a>",
                                                       sep="")) %>% select(-IO_target_URL,-LinkText) %>% 
            DT::datatable(
                options = list(pageLength = 10),
                rownames = FALSE,
                escape = setdiff(colnames(.),"Link to IO Landscape") ## To get hyperlink displayed
            )
    })
}
