


model_selection_module_ui <- function(
    id,
    response_choice_function = get_feature_df_nested_list
){
    ns <- NS(id)
        fluidRow(
            optionsBox(
                width = 12,
                column(
                    width = 4,
                    uiOutput(ns("response_options"))
                ),
                column(
                    width = 4,
                    numericInput(
                        ns("min_mutants"),
                        "Minimum number of mutant samples per group:", 
                        20, 
                        min = 2
                    )
                ),
                column(
                    width = 4,
                    numericInput(
                        ns("min_wildtype"),
                        "Minimum number of wild type samples per group:", 
                        20, 
                        min = 2
                    )
                ),
                column(
                    width = 4,
                    selectInput(
                        ns("group_mode"),
                        "Select mode", 
                        choices = c("By group", "Across groups"),
                        #choices = c("Across groups"),
                        selected = "Across groups"
                    )
                ),
                column(
                    width = 4,
                    checkboxInput(
                        ns("scale_response"),
                        "Scale Response?"
                    )
                ),
                column(
                    width = 12,
                    insert_remove_element_module_ui(ns("select_numeric_feature"), "Add numerical covariate")
                ),
                column(
                    width = 12,
                    insert_remove_element_module_ui(ns("select_categorical_feature"), "Add categorical covariate")
                )
            )
        )
}

model_selection_module <- function(
    input,
    output,
    session,
    numerical_features_named_list,
    categorical_features_named_list
){
    
    ns <- session$ns
    
    output$response_options <- renderUI({
        req(numerical_features_named_list())
        selectInput(
            ns("response_choice_id"),
            "Select Response Variable",
            choices = numerical_features_named_list(),
            selected = "Leukocyte Fraction"
        )
    })
    
    numeric_feature_module <- reactive({
        purrr::partial(
            numeric_model_feature_element,
            feature_named_list = numerical_features_named_list
        )
    })
    
    numeric_feature_module_ui <- reactive(numeric_model_feature_element_ui)
    
    numeric_feature_output <- callModule(
        insert_remove_element_module,
        "select_numeric_feature",
        element_module = numeric_feature_module,
        element_module_ui = numeric_feature_module_ui
    )
    
    
    categorical_feature_module <- reactive({
        purrr::partial(
            categorical_model_feature_element,
            categories_named_list = categorical_features_named_list
        )
    })
    
    categorical_feature_module_ui <- reactive(categorical_model_feature_element_ui)
    
    categorical_feature_output <- callModule(
        insert_remove_element_module,
        "select_categorical_feature",
        element_module = categorical_feature_module,
        element_module_ui = categorical_feature_module_ui
    )
    
    categorical_covariates <- reactive({
        categorical_feature_output() %>% 
            reactiveValuesToList() %>% 
            purrr::discard(purrr::map_lgl(., is.null)) %>% 
            unlist()
    })
    
    numerical_covariates <- reactive({
        numeric_feature_output() %>% 
            reactiveValuesToList() %>% 
            purrr::discard(purrr::map_lgl(., is.null)) %>% 
            purrr::map(purrr::pluck, "feature_choice_id") %>% 
            unlist()
    })
    
    numerical_transformations <- reactive({
        numeric_feature_output() %>% 
            reactiveValuesToList() %>% 
            purrr::discard(purrr::map_lgl(., is.null)) %>% 
            purrr::map(purrr::pluck, "transformation_choice") %>% 
            unlist()
    })
    
    reactive({
        req(
            input$response_variable,
            input$min_mutants,
            input$min_wildtype,
            input$group_mode,
            !is.null(input$scale_response)
        )
        c(
            "response_variable" = input$response_choice_id,
            "min_mutants" = input$min_mutants,
            "min_wildtype" = input$min_wildtype,
            "group_mode" = input$group_mode,
            "scale_response" = input$scale_response,
            "categorical_covariates" = categorical_covariates(),
            "numerical_covariates" = numerical_covariates(),
            "numerical_transformations" = numerical_transformations()
        )
    })
}
