cohort_group_selection_ui <- function(id) {
    
    ns <- shiny::NS(id)
    
    optionsBox(
        width = 12,
        uiOutput(ns("select_group_ui")),
        conditionalPanel(
            condition = "output.display_immune_feature_bins",
            uiOutput(ns("select_immune_feature_bins_group_ui")),
            sliderInput(
                inputId = ns("immune_feature_bin_number"),
                label = "Select number of bins",
                min = 2,
                max = 10,
                value = 2,
                step = 1
            ),
            ns = ns
        ),
        conditionalPanel(
            condition = "output.display_driver_mutation",
            uiOutput(ns("select_driver_mutation_group_ui")),
            ns = ns
        ),
    )
}