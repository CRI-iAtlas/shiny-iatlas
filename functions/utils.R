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

add_transformation_string_to_feature <- function(transformation, feature){
    switch(
        transformation,
        "None" = feature,
        "Log2" = stringr::str_c("Log2( ", feature, " )"),
        "Log2 + 1" = stringr::str_c("Log2( ", feature,  " + 1 )"),
        "Log10" = stringr::str_c("Log10( ",  feature,  " )"),
        "Log10 + 1" = stringr::str_c("Log10( ", feature, " + 1 )")
    )
}


# eventdata utils -------------------------------------------------------------

create_immune_features_heatmap_text <- function(eventdata, con){
    clicked_group <- eventdata %>% 
        dplyr::pull("x") %>% 
        unique
    con %>% 
        dplyr::filter(group == clicked_group) %>% 
        dplyr::mutate(text = paste0(group_name, ": ", characteristics)) %>%
        dplyr::pull(text)
}




# connection/tibble/dataframe checkers ----------------------------------------

assert_df_has_columns <- function(df, columns){
    missing_columns <- columns[!columns %in% colnames(df)]
    if(length(missing_columns) != 0){
        stop("df has missing columns: ",
             stringr::str_c(missing_columns, collapse = ", "))
    }
}

assert_df_has_rows <- function(df){
    if(nrow(df) == 0){
        stop("result df is empty")
    }
}

