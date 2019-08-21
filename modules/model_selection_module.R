create_covariate_ui <- function(
    ui_number,
    remove_button_id,
    ns_func
){
    fluidRow(
        column(
            width = 4,
            selectInput(
                ns_func(stringr::str_c("covariate", ui_number)),
                label = "Select Covariate:",
                choices = get_covariate_nested_list()
            )
        ),
        column(
            width = 4,
            selectInput(
                ns_func(stringr::str_c("transformation", ui_number)),
                label = "Select Transformation:",
                choices = c("None", "Squared", "Log10", "Reciprocal")
            )
        ),
        column(
            width = 4,
            style = "margin-top: 25px;",
            actionButton(ns_func(remove_button_id), 'Remove')
        )
    )
}

output_covariate_selections <- function(input, ui_indeces){
    if(is.null(ui_indeces)){
        res <- list( "covariates" = NULL, "transformations" = NULL)
    } else {
        cov_names <- stringr::str_c("covariate", ui_indeces)
        trans_names <- stringr::str_c("transformation", ui_indeces)
        purrr::map(c(cov_names, trans_names), ~req(input[[.x]]))
        covs <- purrr::map_chr(cov_names,  ~input[[.x]])
        trans <- purrr::map_chr(trans_names, ~input[[.x]])
        res <- list("covariates" = covs, "transformations" = trans)
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
                    insert_remove_element_module_ui(ns("test"))
                )
            )
        )
}

model_selection_module <- function(
    input,
    output,
    session
){
    module_output <- callModule(
        insert_remove_element_module,
        "test",
        create_covariate_ui,
        output_covariate_selections
    )
    
    reactive({
        req(
            input$response_variable,
            input$min_mutants,
            input$min_wildtype,
            input$group_mode,
            !is.null(input$scale_response),
            module_output()
        )
       c(
            "response_variable" = input$response_variable,
            "min_mutants" = input$min_mutants,
            "min_wildtype" = input$min_wildtype,
            "group_mode" = input$group_mode,
            "scale_response" = input$scale_response,
            module_output()
        )
    })
}
