model_selection_ui <- function(id){
    
    ns <- shiny::NS(id)
    source("modules/ui/submodules/insert_remove_element_ui.R", local = T)
    
    shiny::fluidRow(
        .GlobalEnv$optionsBox(
            width = 12,
            shiny::column(
                width = 12,
                insert_remove_element_ui(
                    ns("select_numeric_covariate"), 
                    "Add numerical covariate"
                )
            ),
            shiny::column(
                width = 12,
                insert_remove_element_ui(
                    ns("select_categorical_covariate"), 
                    "Add categorical covariate"
                )
            )
        )
    )
}