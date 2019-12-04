


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
                    insert_remove_element_module_ui(ns("numerical"), "Add numerical covariate")
                ),
                column(
                    width = 12,
                    insert_remove_element_module_ui(ns("categorical"), "Add categorical covariate")
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
            ns("response_variable"),
            "Select Response Variable",
            choices = numerical_features_named_list(),
            selected = "Leukocyte Fraction"
        )
    })
    
    numerical_ui_function <- reactive({
        purrr::partial(
            create_numerical_covariate_ui,
            variable_choice_func = numerical_features_named_list
        )
    })
    
    numerical_module_output <- callModule(
        insert_remove_element_module,
        "numerical",
        ui_creation_function = numerical_ui_function,
        output_function = reactive(output_numerical_covariate_selections)
    )
    
    categorical_ui_function <- reactive({
        # if(input$group_mode == "Across groups") {
        #     choice_function <- 
        #         get_categorical_covariate_nested_list_with_sample_groups
        # } else if (input$group_mode == "By group"){
        #     choice_function <- 
        #         get_categorical_covariate_nested_list
        # }
        purrr::partial(
            create_categorical_covariate_ui,
            variable_choice_func = categorical_features_named_list
        )
    })
    
    catgorical_module_output <- callModule(
        insert_remove_element_module,
        "categorical",
        ui_creation_function = categorical_ui_function,
        output_function = reactive(output_categorical_covariate_selections),
        remove_ui_event = reactive(input$group_mode)
    )
    
    reactive({
        req(
            input$response_variable,
            input$min_mutants,
            input$min_wildtype,
            input$group_mode,
            !is.null(input$scale_response)
        )
        c(
            "response_variable" = input$response_variable,
            "min_mutants" = input$min_mutants,
            "min_wildtype" = input$min_wildtype,
            "group_mode" = input$group_mode,
            "scale_response" = input$scale_response,
            numerical_module_output(),
            catgorical_module_output()
        )
    })
}
