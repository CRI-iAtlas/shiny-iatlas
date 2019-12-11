numeric_filter_element_module_ui <- function(id){
    ns <- NS(id)
    tagList(
        uiOutput(ns("select_ui")),
        uiOutput(ns("slider_ui"))
    )
}

numeric_filter_element_module <- function(
    input,
    output,
    session,
    reactive_values,
    module_id,
    feature_names_list,
    feature_values_con
){

    ns <- session$ns
    
    output$select_ui <- renderUI({
        req(feature_names_list())
        selectInput(
            inputId = ns("feature_choice"),
            label = "Select feature:",
            choices = feature_names_list()
        )
    })
    
    output$slider_ui <- renderUI({
        req(feature_values_con(), input$feature_choice)
        tbl <- feature_values_con() %>% 
            dplyr::filter(feature == local(input$feature_choice)) %>% 
            dplyr::as_tibble() %>% 
            dplyr::filter(
                !is.na(value),
                !is.infinite(value)
            ) %>% 
            dplyr::summarise(mx = max(value), mn = min(value))
        print(tbl)
        min <- tbl$mn
        max <- tbl$mx
        
        sliderInput(
            inputId = ns("numeric_filter_range"),
            label = "Filter:",
            min = round(min, 2),
            max = round(max, 2),
            value = c(min, max)
        )
    })
    
    observeEvent(input$numeric_filter_choice, {
        reactive_values[[module_id]]$variable <- input$numeric_filter_choice
    })
    
    observeEvent(input$numeric_filter_range, {
        reactive_values[[module_id]]$range    <- input$numeric_filter_range
    })
    
    return(reactive_values)
}
