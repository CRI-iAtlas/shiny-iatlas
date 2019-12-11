# These functions return a ui element -----------------------------------------

# used by cohort selection module
create_numerical_filter_ui1 <- function(
    ui_number,
    remove_button_id,
    ns_func,
    variable_choice_func
){
    fluidRow(
        column(
            width = 4,
            style = "margin-top: 25px;",
            actionButton(ns_func(remove_button_id), 'Remove')
        ),
        column(
            width = 8,
            selectInput(
                inputId = ns_func(stringr::str_c("numeric_filter_choice_", ui_number)),
                label = "Select filter:",
                choices = variable_choice_func()
            )
        )
    )
    
}

# used by cohort selection module
create_numerical_filter_ui2 <- function(
    ui_number,
    remove_button_id,
    ns_func,
    input,
    feature_values_con
){
    print(input)
    x <- stringr::str_c("numeric_filter_choice_", ui_number)
    print(x)
    feature = input[[x]]
    print(feature)
    if(!is.null(feature)){
        tbl <- feature_values_con() %>% 
            dplyr::filter(feature == local(input[[x]])) %>% 
            dplyr::summarise(mx = max(value), mn = min(value)) %>% 
            dplyr::as_tibble()
        min <- tbl$mn
        max <- tbl$mx
    } else {
        min <- 0.0
        max <- 1.0
    }
    
    fluidRow(
        column(
            width = 12,
            sliderInput(
                inputId = ns_func(stringr::str_c("numeric_filter_ranges_", ui_number)),
                label = "Filter:",
                min = min,
                max = max,
                value = c(min, max)
            )
        )
    )
}


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