
# misc ------------------------------------------------------------------------
get_unique_values_from_column <- function(con, col){
    con %>% 
        dplyr::select(col) %>% 
        dplyr::distinct() %>% 
        dplyr::pull()
}

create_named_list <- function(con, display_col, internal_col){
    con %>%  
        dplyr::select(d_col = display_col, i_col = internal_col) %>%
        dplyr::filter_all(dplyr::all_vars(!is.na(.))) %>% 
        dplyr::distinct() %>% 
        dplyr::arrange(d_col) %>% 
        dplyr::as_tibble() %>% 
        tibble::deframe()
}

create_nested_named_list <- function(
    con,
    names_col1 = "class",
    names_col2 = "display",
    values_col = "feature"
){
    con %>% 
        dplyr::select(n1 = names_col1, n2 = names_col2, v = values_col) %>% 
        dplyr::filter(!is.na(n1)) %>% 
        dplyr::as_tibble() %>% 
        tidyr::nest(data = c(n2, v)) %>%
        dplyr::mutate(data = purrr::map(data, tibble::deframe)) %>%
        tibble::deframe()
}

translate_value <- function(con, value, from_col, to_col){
    con %>% 
        dplyr::select(from = from_col, to = to_col) %>%
        dplyr::filter(from == value) %>% 
        dplyr::pull(to)
}

translate_values <- function(con, values, from_col, to_col){
    con %>% 
        dplyr::select(from = from_col, to = to_col) %>%
        dplyr::filter(from %in% values) %>% 
        dplyr::pull(to)
}

transform_feature_string <- function(feature, transformation){
    switch(
        transformation,
        "None" = feature,
        "Log2" = stringr::str_c("Log2( ", feature, " )"),
        "Log2 + 1" = stringr::str_c("Log2( ", feature,  " + 1 )"),
        "Log10" = stringr::str_c("Log10( ",  feature,  " )"),
        "Log10 + 1" = stringr::str_c("Log10( ", feature, " + 1 )"),
        "Squared" = stringr::str_c(feature, "**2"),
        "Reciprocal" = stringr::str_c("1/", feature)
    ) 
}

transform_feature_formula <- function(feature, transformation){
    switch(
        transformation,
        "None" = feature,
        "Squared" = stringr::str_c("I(", feature, "**2)"),
        "Log10" = stringr::str_c("I(log10(", feature, "))"),
        "Reciprocal" = stringr::str_c("I(1/", feature, ")")
    )
}

# connection/tibble/dataframe checkers ----------------------------------------

assert_data_has_columns <- function(data, columns){
    missing_columns <- columns[!columns %in% colnames(data)]
    if(length(missing_columns) != 0){
        stop("data has missing columns: ",
             stringr::str_c(missing_columns, collapse = ", "))
    }
}

assert_data_has_rows <- function(data){
    n_rows <- data %>% 
        dplyr::tally() %>% 
        dplyr::pull(n) %>% 
        as.integer()
    if(n_rows == 0L){
        stop("data is empty")
    }
}

# other -----------------------------------------------------------------------

calculate_lm_pvalue <- function(tbl, lm_formula, term){
    tbl %>% 
        lm(formula = lm_formula, data = .) %>% 
        summary %>% 
        magrittr::use_series(coefficients) %>% 
        .[term, "Pr(>|t|)"] %>% 
        as.double()
}

get_effect_size_from_df <- function(df, method){
    method(unlist(df$GROUP1), unlist(df$GROUP2))
}

ratio_effect_size <- function(v1, v2){
    mean1 <- mean(v1)
    mean2 <- mean(v2)
    if(any(mean1 <= 0, mean2 <= 0)) return(NA)
    mean1 / mean2
}
