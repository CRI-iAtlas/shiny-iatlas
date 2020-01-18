# used in cohort selection ----------------------------------------------------

numeric_filter_element_ui <- function(id){
    ns <- NS(id)
    tagList(
        uiOutput(ns("select_ui")),
        uiOutput(ns("slider_ui"))
    )
}

group_filter_element_ui <- function(id){
    ns <- NS(id)
    tagList(
        uiOutput(ns("select_ui")),
        uiOutput(ns("checkbox_ui"))
    )
}

# used in driver module -------------------------------------------------------

numeric_model_covariate_element_ui <- function(id){
    ns <- NS(id)
    tagList(
        uiOutput(ns("select_covariate_ui")),
        uiOutput(ns("select_transformation_ui"))
    )
}


categorical_model_covariate_element_ui <- function(id){
    ns <- NS(id)
    tagList(
        uiOutput(ns("select_covariate_ui"))
    )
}