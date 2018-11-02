###############################################################################
# These functions have been refactored and have unit tests.
# Do not make any modifications to these!
# If you want to make a modification, please copy and paste the function the
# lower section and call it <function_name>2.
# Make any needed modifcations to the coipied function.
# The new functionality will get unut tests and be folded back into the 
# original function.
###############################################################################

# assert_df_has_columns ------------------------------------------------------------

assert_df_has_columns <- function(df, columns){
    missing_columns <- columns[!columns %in% colnames(df)]
    if(length(missing_columns) != 0){
        stop("df has missing columns: ",
             str_c(missing_columns, collapse = ", "))
    }
}

# assert_df_has_rows ----------------------------------------------------------

assert_df_has_rows <- function(df){
    if(nrow(df) == 0){
        stop("result df is empty")
    }
}

# convert_values --------------------------------------------------------------

convert_values <- function(values, df, from_column, to_column){
    assert_df_has_columns(df, c(from_column, to_column))
    df %>% 
        dplyr::select(FROM = from_column, TO = to_column) %>% 
        dplyr::filter(FROM %in% values) %>% 
        magrittr::use_series(TO)
}


# convert_value_between_columns -----------------------------------------------

convert_value_between_columns <- function(
    input_value, df, from_column, to_column,
    no_matches = "error",
    many_matches = "error"){
    
    result <- convert_values(
        input_value, df, from_column, to_column)
    
    # 1 match
    if (length(result) == 1) return(result) 
    
    # no matches
    if (length(result) == 0){
        if(no_matches == "return_input"){
            return(input_value)
        } else if(no_matches == "return_na") {
            return(NA)
        } else {
            stop("input value has no matches: ", input_value)
        }
    }
    
    # many matches
    if (length(result) > 1){
        if(many_matches == "return_result"){
            return(result)
        } else {
            stop("input value: ",
                 input_value, 
                 ", has multiple matches: ", 
                 str_c(result, collapse = ", "))
        }
    }
}


get_group_internal_name <- purrr::partial(
    convert_value_between_columns,
    df = panimmune_data$feature_df,
    to_column = "FeatureMatrixLabelTSV",
    from_column = "FriendlyLabel",
    no_matches = "return_input")

get_variable_display_name <- purrr::partial(
    convert_value_between_columns,
    df = panimmune_data$feature_df,
    from_column = "FeatureMatrixLabelTSV",
    to_column = "FriendlyLabel")

get_variable_internal_name <- purrr::partial(
    convert_value_between_columns,
    df = panimmune_data$feature_df,
    to_column = "FeatureMatrixLabelTSV",
    from_column = "FriendlyLabel")

get_im_display_name <- purrr::partial(
    convert_value_between_columns,
    df = panimmune_data$im_direct_relationships,
    to_column = "Gene",
    from_column = "HGNC Symbol")

get_variable_internal_names <- purrr::partial(
    convert_value_between_columns,
    df = panimmune_data$feature_df,
    to_column = "FeatureMatrixLabelTSV",
    from_column = "FriendlyLabel",
    many_matches = "return_result")


# convert_values_between_columns ----------------------------------------------

convert_values_between_columns <- function(values, df, from_column, to_column){
    results <- values %>% 
        convert_value_between_columns(
            df, 
            from_column, 
            to_column,
            no_matches = "return_na",
            many_matches = "return_result") %>% 
        purrr::compact() %>% 
        purrr::discard(~is.na(.))
}



# get_complete_df_by_columns --------------------------------------------------

get_complete_df_by_columns <- function(df, columns){
    assert_df_has_columns(df, columns)
    result_df <- df %>%
        dplyr::select(columns) %>%
        tidyr::drop_na() 
    assert_df_has_rows(result_df)
    return(result_df)
}

# get_complete_class_df -------------------------------------------------------

get_complete_class_df <- function(
    class_name, df, class_column, variable_column, order_column){
    
    columns <- c(class_column, variable_column, order_column)
    assert_df_has_columns(df, columns)
    
    result_df <- df %>% 
        dplyr::select(CLASS = class_column, variable_column, order_column) %>% 
        get_complete_df_by_columns(c(
            "CLASS",
            variable_column,
            order_column)) %>% 
        dplyr::filter(CLASS == class_name) %>% 
        dplyr::select(variable_column, order_column)
    assert_df_has_rows(result_df)
    return(result_df)
}

# factor_variables_with_df ----------------------------------------------------

factor_variables_with_df <- function(df, variable_column, order_column){
    columns <- c(variable_column, order_column)
    assert_df_has_columns(df, columns)

    result_df <- df %>% 
        get_complete_df_by_columns(columns) %>% 
        dplyr::select(VAR = variable_column, ORDER = order_column) %>%
        dplyr::arrange(ORDER)
    
    assert_df_has_rows(result_df)
    
    result_df %>% 
        magrittr::use_series(VAR) %>% 
        factor(., levels = .)
}



# get_factored_variables_by_class ---------------------------------------------

get_factored_variables_by_class <- function(
    class_name, df, class_column, variable_column, order_column){
    
    class_name %>% 
        get_complete_class_df(df, class_column, variable_column, order_column) %>% 
        factor_variables_with_df(variable_column, order_column)
}

get_factored_variables_from_feature_df <- purrr::partial(
    get_factored_variables_by_class,
    df = panimmune_data$feature_df,
    class_column = "Variable Class",
    variable_column = "FeatureMatrixLabelTSV",
    order_column = "Variable Class Order" 
)


# df_to_nested_list -----------------------------------------------------------

df_to_nested_list <- function(df, group_column, key_column, value_column){
    df %>% 
        get_complete_df_by_columns(c(group_column, value_column, key_column)) %>% 
        tidyr::nest(-1) %>%
        dplyr::mutate(data = purrr::map(data, tibble::deframe)) %>%
        tibble::deframe()
}

# get_column_names_of_type ----------------------------------------------------

get_column_names_of_type <- function(df, func){
    col_names <- df %>% 
        dplyr::select_if(func) %>% 
        colnames() 
    if(length(col_names) == 0) {
        stop("df has no columns from selection function")
    }
    return(col_names)
}

# create_nested_list_by_class -------------------------------------------------

create_nested_list_by_class <- function(
    df, 
    class_column = "CLASS",
    display_column = "DISPLAY",
    internal_column = "INTERNAL"){
    
    df %>%
        dplyr::select(
            CLASS = class_column,
            DISPLAY = display_column,
            INTERNAL = internal_column) %>%
        dplyr::mutate(CLASS = ifelse(is.na(CLASS), "Other", CLASS)) %>%
        df_to_nested_list(
            group_column = "CLASS",
            key_column = "INTERNAL",
            value_column = "DISPLAY")
}

get_immunomodulator_nested_list <- purrr::partial(
    create_nested_list_by_class,
    df = panimmune_data$im_direct_relationships,
    display_column = "Gene",
    internal_column = "HGNC Symbol"
)

# get_nested_list_by_column_type ----------------------------------------------


create_filtered_nested_list_by_class <- function(
    feature_df,
    filter_value,
    class_column = "CLASS",
    display_column = "DISPLAY",
    internal_column = "INTERNAL",
    filter_column = "FILTER"){
    
    feature_df %>%
        dplyr::select(
            CLASS = class_column,
            DISPLAY = display_column,
            INTERNAL = internal_column,
            FILTER = filter_column) %>% 
        dplyr::filter(FILTER == filter_value) %>%
        select(CLASS, INTERNAL, DISPLAY) %>% 
        create_nested_list_by_class()
}

get_feature_df_nested_list <- purrr::partial(
    create_filtered_nested_list_by_class,
    feature_df = panimmune_data$feature_df,
    filter_value = "Numeric",
    class_column = "Variable Class",
    internal_column = "FeatureMatrixLabelTSV",
    display_column = "FriendlyLabel",
    filter_column = "VariableType"
)

# get_unique_column_values ----------------------------------------------------

get_unique_column_values <- function(column, df){
    assert_df_has_columns(df, column)
    df %>% 
        magrittr::extract2(column) %>% 
        na.omit() %>%
        unique() %>%
        sort() %>%
        as.character()
}

# get_variable_classes --------------------------------------------------------

get_variable_classes <- function(df, class_column, type_column, value){
    df %>% 
        dplyr::select(CLASS = class_column, TYPE = type_column) %>% 
        dplyr::filter(TYPE == value) %>% 
        magrittr::use_series(CLASS) %>% 
        unique %>% 
        purrr::discard(is.na(.))
}

get_numeric_classes_from_feature_df <- purrr::partial(
    get_variable_classes,
    df = panimmune_data$feature_df,
    class_column = "Variable Class",
    type_column = "VariableType", 
    value = "Numeric"
)



###############################################################################
# Tests below this line do not have tests yet, newly writen functions 
###############################################################################

se <- function(x){
    mean(x) / sqrt(length(x))
}   
