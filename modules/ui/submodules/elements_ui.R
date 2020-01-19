# used in cohort selection ----------------------------------------------------

numeric_filter_element_ui <- function(id){
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::uiOutput(ns("select_ui")),
        shiny::uiOutput(ns("slider_ui"))
    )
}

group_filter_element_ui <- function(id){
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::uiOutput(ns("select_ui")),
        shiny::uiOutput(ns("checkbox_ui"))
    )
}

# used in driver module -------------------------------------------------------

numeric_model_covariate_element_ui <- function(id){
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::uiOutput(ns("select_covariate_ui")),
        shiny::uiOutput(ns("select_transformation_ui"))
    )
}


categorical_model_covariate_element_ui <- function(id){
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::uiOutput(ns("select_covariate_ui"))
    )
}