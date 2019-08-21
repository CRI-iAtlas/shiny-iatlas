insert_remove_element_module_ui <- function(
    id, 
    button_label = "Add UI element"
){
    ns <- NS(id)
    actionButton(ns("add_button"), button_label)
}

insert_remove_element_module <- function(
    input,
    output,
    session, 
    ui_creation_function, 
    output_function
){
    
    params <- reactiveValues(current_ui_indeces = c())
    
    ns <- session$ns
    observeEvent(input$add_button, {
        button_n <- input$add_button
        params$current_ui_indeces <- c(params$current_ui_indeces, button_n)
        remove_button_id <- ns(paste0('remove_button', button_n))
        ui_div_id <- ns(paste0("new_input", button_n))
        insertUI(
            selector = paste0("#", ns("add_button")),
            where = "afterEnd",
            ui = div(
                id = ui_div_id,
                ui_creation_function(button_n, remove_button_id, ns)
            )
        )
        observeEvent(input[[remove_button_id]], {
            shiny::removeUI(selector = paste0("#", ui_div_id))
            params$current_ui_indeces <- params$current_ui_indeces %>% 
                purrr::discard(. == button_n)
        })
    })
    
    reactive(output_function(input, params$current_ui_indeces))
}