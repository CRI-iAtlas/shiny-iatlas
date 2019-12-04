# These functions return a ui element -----------------------------------------

# used by model selection model
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

# used by model selection model
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
# These functions are sent to insert/create elemet module to return a result
# from the session input

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