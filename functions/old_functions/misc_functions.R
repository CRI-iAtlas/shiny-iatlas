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