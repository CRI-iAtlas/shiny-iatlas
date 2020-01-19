insert_remove_element_server <- function(
    input,
    output,
    session, 
    element_module,
    element_module_ui,
    remove_ui_event = reactive(NULL)
){

    params  <- shiny::reactiveValues(ui_numbers = c())
    results <- shiny::reactiveValues()
        
    ns <- session$ns
   
    shiny::observeEvent(input$add_button, {

        ui_number           <- input$add_button
        params$ui_numbers   <- c(params$ui_numbers, ui_number)
        remove_button_id    <- stringr::str_c('remove_button', ui_number)
        ui_id               <- ns(stringr::str_c("ui", ui_number))
        add_button_selector <- stringr::str_c("#", ns("add_button"))
        ui_selector         <- stringr::str_c("#", ui_id)
        module_id           <- stringr::str_c("element", ui_number)
        
        shiny::insertUI(
            selector = add_button_selector,
            where = "afterEnd",
            ui = shiny::div(
                id = ui_id,
                shiny::actionButton(ns(remove_button_id), 'Remove')
            )
        )
        shiny::insertUI(
            selector = add_button_selector,
            where = "afterEnd",
            ui = shiny::div(
                id = ui_id,
                element_module_ui()(ns(module_id))
            )
        )
        
        results <- shiny::callModule(
            element_module(), 
            module_id, results, 
            module_id
        )

        shiny::observeEvent(input[[remove_button_id]], {
            shiny::removeUI(selector = ui_selector)
            shiny::removeUI(selector = ui_selector)
            results[[module_id]] <- NULL
            params$ui_numbers <- params$ui_numbers %>%
                purrr::discard(. == ui_number)
        })
    })

    shiny::observeEvent(remove_ui_event(), {
        button_selectors <-
            stringr::str_c("ui", params$ui_numbers) %>%
            ns() %>%
            stringr::str_c("#", .)
        purrr::walk(button_selectors, removeUI)
        purrr::walk(button_selectors, removeUI)
        params$ui_numbers <- c()
        results <- shiny::reactiveValues()
    })

    return(shiny::reactive(results))
}
