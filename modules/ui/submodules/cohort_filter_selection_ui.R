cohort_filter_selection_ui <- function(id) {
    
    ns <- shiny::NS(id)
    
    source("modules/ui/submodules/insert_remove_element_ui.R", local = T)
    
    shiny::tagList(
        optionsBox(
            width = 12,
            insert_remove_element_ui(
                ns("group_filter"), 
                "Add group filter"
            )
        ),
        optionsBox(
            width = 12,
            insert_remove_element_ui(
                ns("numeric_filter"), 
                "Add numeric filter"
            )
        ),
        tableBox(
            width = 12,
            textOutput(ns("samples_text"))
        )
    )

}