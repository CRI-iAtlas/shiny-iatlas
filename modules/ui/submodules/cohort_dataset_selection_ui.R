cohort_dataset_selection_ui <- function(id) {
    
    ns <- shiny::NS(id)
    
    shiny::tagList(
        .GlobalEnv$optionsBox(
            width = 4,
            shiny::checkboxInput(
                inputId = ns("select_by_module"),
                label = shiny::strong("Select By Module?"),
            ),
            shiny::conditionalPanel(
                condition = "input.select_by_module",
                shiny::uiOutput(ns("module_selection_ui")),
                ns = ns
            ),
            shiny::uiOutput(ns("dataset_selection_ui")),
        ),
        .GlobalEnv$messageBox(
            width = 12,
            shiny::textOutput(ns("module_availibility_string"))
        )
    )
}