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
    remove_ui_event = reactive(NULL)
){
    params <- reactiveValues(ui_numbers = c())
    
    ns <- session$ns
    observeEvent(input$add_button, {
        
        ui_number           <- input$add_button
        params$ui_numbers   <- c(params$ui_numbers, ui_number)
        remove_button_id    <- ns(stringr::str_c('remove_button', ui_number))
        ui_id               <- ns(stringr::str_c("ui", ui_number))
        add_button_selector <- stringr::str_c("#", ns("add_button"))
        ui_selector         <- stringr::str_c("#", ui_id)
        
        insertUI(
            selector = add_button_selector,
            where = "afterEnd",
            ui = div(
                id = ui_id,
                ui_creation_function()(
                    ui_number = ui_number, 
                    remove_button_id = remove_button_id, 
                    ns_func = ns
                )
            )
        )
        observeEvent(input[[remove_button_id]], {
            shiny::removeUI(selector = ui_selector)
            params$ui_numbers <- params$ui_numbers %>% 
                purrr::discard(. == ui_number)
        })
    })
    
    observeEvent(remove_ui_event(), {
        button_selectors <-
            stringr::str_c("ui", params$ui_numbers) %>%
            ns() %>%
            stringr::str_c("#", .) %>%
            purrr::walk(removeUI)
        params$ui_numbers <- c()
    })
    
    reactive(output_function()(input, params$ui_numbers))
}