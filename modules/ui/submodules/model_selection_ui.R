model_selection_ui <- function(id){
    ns <- NS(id)
    fluidRow(
        optionsBox(
            width = 12,
            column(
                width = 12,
                insert_remove_element_ui(
                    ns("select_numeric_covariate"), 
                    "Add numerical covariate"
                )
            ),
            column(
                width = 12,
                insert_remove_element_ui(
                    ns("select_categorical_covariate"), 
                    "Add categorical covariate"
                )
            )
        )
    )
}