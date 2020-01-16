# used in cohort selection ----------------------------------------------------

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
            label = "Select or Search for feature",
            choices = feature_names_list()
        )
    })
    
    output$slider_ui <- renderUI({
        req(feature_values_con(), input$feature_choice)
        tbl <- feature_values_con() %>% 
            dplyr::filter(feature_id == local(input$feature_choice)) %>% 
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
        reactive_values[[module_id]]$feature_choice <- input$feature_choice
    })
    
    observeEvent(input$range, {
        reactive_values[[module_id]]$feature_range <- input$range
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
            label = "Select or Search for Group",
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

# used in driver module -------------------------------------------------------

numeric_model_covariate_element_ui <- function(id){
    ns <- NS(id)
    tagList(
        uiOutput(ns("select_covariate_ui")),
        uiOutput(ns("select_transformation_ui"))
    )
}

numeric_model_covariate_element <- function(
    input,
    output,
    session,
    reactive_values,
    module_id,
    covariate_tbl
){
    
    ns <- session$ns
    
    output$select_covariate_ui <- renderUI({
        req(covariate_tbl())
        choices <- create_nested_named_list(covariate_tbl())
        selectInput(
            inputId = ns("covariate_choice_id"),
            label = "Select or Search for Covariate",
            choices = choices
        )
    })
    
    output$select_transformation_ui <- renderUI({
        selectInput(
            inputId = ns("transformation_choice"),
            label = "Select or Search for Transformation",
            choices = c("None", "Squared", "Log10", "Reciprocal")
        )
    })
    
    observeEvent(input$covariate_choice_id, {
        reactive_values[[module_id]]$covariate_choice_id <- as.numeric(input$covariate_choice_id)
    })
    
    observeEvent(input$transformation_choice, {
        reactive_values[[module_id]]$transformation_choice <- input$transformation_choice
    })
    
    return(reactive_values)
}

categorical_model_covariate_element_ui <- function(id){
    ns <- NS(id)
    tagList(
        uiOutput(ns("select_covariate_ui"))
    )
}

categorical_model_covariate_element <- function(
    input,
    output,
    session,
    reactive_values,
    module_id,
    covariate_tbl
){
    
    ns <- session$ns
    
    output$select_covariate_ui <- renderUI({
        req(covariate_tbl())
        choices <- create_nested_named_list(covariate_tbl())
        selectInput(
            inputId = ns("covariate_choice"),
            label = "Select or Search for Covariate",
            choices = choices
        )
    })
    
    observeEvent(input$covariate_choice, {
        reactive_values[[module_id]]$covariate_choice <- input$covariate_choice
    })
    
    return(reactive_values)
}
