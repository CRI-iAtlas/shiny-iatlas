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
        min <- tbl$mn
        max <- tbl$mx
        
        sliderInput(
            inputId = ns("range"),
            label = "Filter:",
            min = round(min, 2),
            max = round(max, 2),
            value = c(min, max)
        )
    })
    
    observeEvent(input$feature_choice, {
        reactive_values[[module_id]]$variable <- input$feature_choice
    })
    
    observeEvent(input$range, {
        reactive_values[[module_id]]$range <- input$range
    })
    
    return(reactive_values)
}

group_filter_element_module_ui <- function(id){
    ns <- NS(id)
    tagList(
        uiOutput(ns("select_ui")),
        uiOutput(ns("checkbox_ui"))
    )
}

group_filter_element_module <- function(
    input,
    output,
    session,
    reactive_values,
    module_id,
    group_names_list,
    group_values_con
){
    
    ns <- session$ns
    
    output$select_ui <- renderUI({
        req(group_names_list())
        selectInput(
            inputId = ns("parent_group_choice"),
            label = "Select group:",
            choices = group_names_list()
        )
    })
    
    output$checkbox_ui <- renderUI({
        req(group_values_con(), input$parent_group_choice)
        choices <- group_values_con() %>% 
            dplyr::filter(parent_group == local(input$parent_group_choice)) %>% 
            dplyr::pull(group)
        
        checkboxGroupInput(
            inputId = ns("group_choices"),
            label = "Select choices to include:",
            choices = choices,
            inline = T
        )
    })
    
    observeEvent(input$parent_group_choice, {
        reactive_values[[module_id]]$parent_group_choice <- input$parent_group_choice
    })
    
    observeEvent(input$group_choices, {
        reactive_values[[module_id]]$group_choices <- input$group_choices
    })
    
    return(reactive_values)
}
