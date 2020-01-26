# used in cohort selection ----------------------------------------------------

numeric_filter_element_server <- function(
    input,
    output,
    session,
    reactive_values,
    module_id,
    feature_named_list
){

    ns <- session$ns
    
    output$select_ui <- shiny::renderUI({
        shiny::req(feature_named_list())
        shiny::selectInput(
            inputId = ns("feature_choice"),
            label = "Select or Search for feature",
            choices = feature_named_list()
        )
    })
    
    features_tbl <- shiny::reactive({
        req(input$feature_choice)
        input$feature_choice %>% 
            .GlobalEnv$create_feature_value_query() %>% 
            .GlobalEnv$perform_query("build features table") %>% 
            dplyr::filter(!is.na(value), !is.infinite(value)) %>% 
            dplyr::summarise(mx = max(value), mn = min(value)) 
    })
    
    output$slider_ui <- shiny::renderUI({
        shiny::req(features_tbl())
        shiny::sliderInput(
            inputId = ns("range"),
            label = "Filter:",
            min = round(features_tbl()$mn, 2),
            max = round(features_tbl()$mx, 2),
            value = c(features_tbl()$mn, features_tbl()$mx)
        )
    })
    
    shiny::observeEvent(input$feature_choice, {
        reactive_values[[module_id]]$feature_choice <- input$feature_choice
    })
    
    shiny::observeEvent(input$range, {
        reactive_values[[module_id]]$feature_range <- input$range
    })
    
    return(reactive_values)
}

group_filter_element_server <- function(
    input,
    output,
    session,
    reactive_values,
    module_id,
    group_named_list
){
    
    ns <- session$ns
    
    output$select_ui <- shiny::renderUI({
        shiny::req(group_named_list())
        shiny::selectInput(
            inputId = ns("parent_group_choice_id"),
            label = "Select or Search for Group",
            choices = group_named_list()
        )
    })
    
    output$checkbox_ui <- shiny::renderUI({
        shiny::req(input$parent_group_choice_id)
        group_id_query <- 
            .GlobalEnv$create_parent_group_query_from_id(
            input$parent_group_choice_id
        ) 
        choices <- paste(
            "SELECT name, id FROM  (", 
            group_id_query,
            ") a"
        ) %>% 
            .GlobalEnv$perform_query("build groups table") %>% 
            tibble::deframe()
        
        shiny::checkboxGroupInput(
            inputId = ns("group_choice_ids"),
            label = "Select choices to include:",
            choices = choices,
            inline = T
        )
    })
    
    shiny::observeEvent(input$group_choice_ids, {
        reactive_values[[module_id]]$group_choice_ids <- input$group_choice_ids
    })
    
    return(reactive_values)
}

# used in driver module -------------------------------------------------------

numeric_model_covariate_element_server <- function(
    input,
    output,
    session,
    reactive_values,
    module_id,
    covariate_tbl
){
    
    ns <- session$ns
    
    output$select_covariate_ui <- shiny::renderUI({
        shiny::req(covariate_tbl())
        choices <- create_nested_named_list(covariate_tbl())
        shiny::selectInput(
            inputId = ns("covariate_choice_id"),
            label = "Select or Search for Covariate",
            choices = choices
        )
    })
    
    output$select_transformation_ui <- shiny::renderUI({
        shiny::selectInput(
            inputId = ns("transformation_choice"),
            label = "Select or Search for Transformation",
            choices = c("None", "Squared", "Log10", "Reciprocal")
        )
    })
    
    shiny::observeEvent(input$covariate_choice_id, {
        reactive_values[[module_id]]$covariate_choice_id <- as.numeric(input$covariate_choice_id)
    })
    
    shiny::observeEvent(input$transformation_choice, {
        reactive_values[[module_id]]$transformation_choice <- input$transformation_choice
    })
    
    return(reactive_values)
}


categorical_model_covariate_element_server <- function(
    input,
    output,
    session,
    reactive_values,
    module_id,
    covariate_tbl
){
    
    ns <- session$ns
    
    output$select_covariate_ui <- shiny::renderUI({
        shiny::req(covariate_tbl())
        choices <- create_nested_named_list(covariate_tbl())
        selectInput(
            inputId = ns("covariate_choice"),
            label = "Select or Search for Covariate",
            choices = choices
        )
    })
    
    shiny::observeEvent(input$covariate_choice, {
        reactive_values[[module_id]]$covariate_choice <- input$covariate_choice
    })
    
    return(reactive_values)
}
