insert_remove_element_ui <- function(
    id, 
    button_label = "Add UI element"
){
    ns <- NS(id)
    actionButton(ns("add_button"), button_label)
}