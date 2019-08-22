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
    output_function,
    remove_buttons_event = reactive(NULL)
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
                ui_creation_function()(
                    ui_number = button_n, 
                    remove_button_id = remove_button_id, 
                    ns_func = ns
                )
            )
        )
        observeEvent(input[[remove_button_id]], {
            shiny::removeUI(selector = paste0("#", ui_div_id))
            params$current_ui_indeces <- params$current_ui_indeces %>% 
                purrr::discard(. == button_n)
        })
    })
    
    observeEvent(remove_buttons_event(), {
        button_selectors <-
            stringr::str_c("new_input", params$current_ui_indeces) %>%
            ns() %>%
            stringr::str_c("#", .) %>%
            purrr::walk(removeUI)
        params$current_ui_indeces <- c()
    })
    
    reactive(output_function()(input, params$current_ui_indeces))
}