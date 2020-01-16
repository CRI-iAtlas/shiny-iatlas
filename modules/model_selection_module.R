


model_selection_module_ui <- function(id){
    ns <- NS(id)
    fluidRow(
        optionsBox(
            width = 12,
            column(
                width = 12,
                insert_remove_element_module_ui(
                    ns("select_numeric_covariate"), 
                    "Add numerical covariate"
                )
            ),
            column(
                width = 12,
                insert_remove_element_module_ui(
                    ns("select_categorical_covariate"), 
                    "Add categorical covariate"
                )
            )
        )
    )
}

model_selection_module <- function(
    input,
    output,
    session,
    numerical_covariate_tbl,
    categorical_covariate_tbl,
    model_string_prefix,
    model_formula_prefix = "response ~ status"
){
    
    ns <- session$ns
    
    numeric_covariate_module <- reactive({
        purrr::partial(
            numeric_model_covariate_element,
            covariate_tbl = numerical_covariate_tbl
        )
    })
    
    numeric_covariate_module_ui <- reactive(numeric_model_covariate_element_ui)
    
    numeric_covariate_output <- callModule(
        insert_remove_element_module,
        "select_numeric_covariate",
        element_module = numeric_covariate_module,
        element_module_ui = numeric_covariate_module_ui
    )
    
    
    categorical_covariate_module <- reactive({
        purrr::partial(
            categorical_model_covariate_element,
            covariate_tbl = categorical_covariate_tbl
        )
    })
    
    categorical_covariate_module_ui <- reactive(categorical_model_covariate_element_ui)
    
    categorical_covariate_output <- callModule(
        insert_remove_element_module,
        "select_categorical_covariate",
        element_module = categorical_covariate_module,
        element_module_ui = categorical_covariate_module_ui
    )
    
    categorical_covariates <- reactive({
        categorical_covariate_output() %>% 
            reactiveValuesToList() %>% 
            purrr::discard(purrr::map_lgl(., is.null)) %>% 
            unlist() %>% 
            unname()
    })
    
    categorical_display_string <- reactive({
        if(!is.null(categorical_covariates())){
            string <- categorical_covariates() %>% 
                purrr::map(~translate_value(
                    categorical_covariate_tbl(),
                    .x,
                    "feature",
                    "display")
                ) %>%
                stringr::str_c(collapse = " + ")
            return(string)
        } else{
            return(NULL)
        }
    })
    
    categorical_formula_string <- reactive({
        if(!is.null(categorical_covariates())){
            string <- categorical_covariates() %>% 
                purrr::map(~translate_value(
                    categorical_covariate_tbl(),
                    .x,
                    "feature",
                    "internal")
                ) %>%
                stringr::str_c(collapse = " + ")
            return(string)
        } else{
            return(NULL)
        }
    })
    
    numerical_covariates <- reactive({
        numeric_covariate_output() %>% 
            reactiveValuesToList() %>% 
            purrr::discard(purrr::map_lgl(., is.null)) %>% 
            purrr::map(purrr::pluck, "covariate_choice_id") %>% 
            unlist() %>% 
            unname()
    })
    
    numerical_transformations <- reactive({
        numeric_covariate_output() %>% 
            reactiveValuesToList() %>% 
            purrr::discard(purrr::map_lgl(., is.null)) %>% 
            purrr::map(purrr::pluck, "transformation_choice") %>% 
            unlist() %>% 
            unname() 
    })
    
    numerical_display_string <- reactive({
        if(!is.null(numerical_covariates())){
            string <- numerical_covariates() %>%
                purrr::map(~translate_value(
                    numerical_covariate_tbl(),
                    .x,
                    "feature",
                    "display")
                ) %>%
                purrr::map2_chr(
                    numerical_transformations(),
                    transform_feature_string
                ) %>%
                stringr::str_c(collapse = " + ") 
            return(string)
        } else{
            return(NULL)
        }
    })
    
    numerical_formula_string <- reactive({
        if(!is.null(numerical_covariates())){
            string <- numerical_covariates() %>%
                purrr::map(~translate_value(
                    numerical_covariate_tbl(),
                    .x,
                    "feature",
                    "internal")
                ) %>%
                purrr::map2_chr(
                    numerical_transformations(),
                    transform_feature_formula
                ) %>%
                stringr::str_c(collapse = " + ") 
            return(string)
        } else{
            return(NULL)
        }
    })
    
    display_string <- reactive({
        req(model_string_prefix())
        
        string <- model_string_prefix()
        if(!is.null(categorical_display_string())){
            string <- stringr::str_c(string, " + ",  categorical_display_string())
        }
        if(!is.null(numerical_display_string())){
            string <- stringr::str_c(string, " + ",  numerical_display_string())
        }
        return(string)
    })
    
    formula_string <- reactive({
        req(model_formula_prefix)
        
        string <- model_formula_prefix
        if(!is.null(categorical_formula_string())){
            string <- stringr::str_c(string, " + ",  categorical_formula_string())
        }
        if(!is.null(numerical_formula_string())){
            string <- stringr::str_c(string, " + ",  numerical_formula_string())
        }
        return(string)
    })
    
    reactive({
        list(
            "categorical_covariates" = categorical_covariates(),
            "numerical_covariates" = numerical_covariates(),
            "numerical_transformations" = numerical_transformations(),
            "display_string" = display_string(),
            "formula_string" = formula_string()
        )
    })
}
