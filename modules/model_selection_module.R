create_numerical_covariate_ui <- function(
    ui_number,
    remove_button_id,
    ns_func,
    variable_choice_func
){
    fluidRow(
        column(
            width = 2,
            style = "margin-top: 25px;",
            actionButton(ns_func(remove_button_id), 'Remove')
        ),
        column(
            width = 5,
            selectInput(
                ns_func(stringr::str_c("covariate", ui_number)),
                label = "Select Covariate:",
                choices = variable_choice_func()
            )
        ),
        column(
            width = 5,
            selectInput(
                ns_func(stringr::str_c("transformation", ui_number)),
                label = "Select Transformation:",
                choices = c("None", "Squared", "Log10", "Reciprocal")
            )
        )
    )
}

create_categorical_covariate_ui <- function(
    ui_number,
    remove_button_id,
    ns_func,
    variable_choice_func = get_categorical_covariate_nested_list
){
    fluidRow(
        column(
            width = 2,
            style = "margin-top: 25px;",
            actionButton(ns_func(remove_button_id), 'Remove')
        ),
        column(
            width = 5,
            selectInput(
                ns_func(stringr::str_c("covariate", ui_number)),
                label = "Select Covariate:",
                choices = variable_choice_func()
            )
        )
    )
}

output_numerical_covariate_selections <- function(input, ui_indeces){
    if(is.null(ui_indeces) | length(ui_indeces) == 0){
        res <- list(
            "numerical_covariates" = NULL,
            "numerical_transformations" = NULL
        )
    } else {
        cov_names <- stringr::str_c("covariate", ui_indeces)
        trans_names <- stringr::str_c("transformation", ui_indeces)
        purrr::map(c(cov_names, trans_names), ~req(input[[.x]]))
        covs <- purrr::map_chr(cov_names,  ~input[[.x]])
        trans <- purrr::map_chr(trans_names, ~input[[.x]])
        res <- list(
            "numerical_covariates" = covs,
            "numerical_transformations" = trans
        )
    }
    return(res)
}

output_categorical_covariate_selections <- function(input, ui_indeces){
    if(is.null(ui_indeces) | length(ui_indeces) == 0){
        res <- list(
            "categorical_covariates" = NULL, 
            "categorical_transformations" = NULL
        )
    } else {
        cov_names <- stringr::str_c("covariate", ui_indeces)
        purrr::map(cov_names, ~req(input[[.x]]))
        covs <- purrr::map_chr(cov_names,  ~input[[.x]])
        trans <- rep("None", length(covs))
        res <- list(
            "categorical_covariates" = covs, 
            "categorical_transformations" = trans
        )
    }
    return(res)
}

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
                    selectInput(
                        ns("response_variable"),
                        "Select Response Variable",
                        choices = response_choice_function()
                    )
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
    session
){
    numerical_module_output <- callModule(
        insert_remove_element_module,
        "numerical",
        reactive(create_numerical_covariate_ui),
        reactive(output_numerical_covariate_selections)
    )
    
    categorical_ui_function <- reactive({
        if(input$group_mode == "Across groups") {
            choice_function <- 
                get_categorical_covariate_nested_list_with_sample_groups
        } else if (input$group_mode == "By group"){
            choice_function <- 
                get_categorical_covariate_nested_list
        }
        purrr::partial(
            create_categorical_covariate_ui,
            variable_choice_func = choice_function
        )
    })
    
    catgorical_module_output <- callModule(
        insert_remove_element_module,
        "categorical",
        categorical_ui_function,
        reactive(output_categorical_covariate_selections),
        reactive(input$group_mode)
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
