get_leukocyte_fraction_id <- function(){
    query <- "SELECT id FROM features WHERE display = 'Leukocyte Fraction'"
    query %>%
        dplyr::sql() %>%
        .GlobalEnv$perform_query("get feature id") %>%
        dplyr::pull(id)
}

get_dna_alteration_class_id <- function(){
    query <- "SELECT id FROM classes WHERE name = 'DNA Alteration'"
    query %>%
        dplyr::sql() %>%
        .GlobalEnv$perform_query("get class id") %>%
        dplyr::pull(id)
}

create_class_list <- function(){
    query <- "SELECT name, id FROM classes"
    query %>%
        dplyr::sql() %>%
        .GlobalEnv$perform_query("get class id") %>%
        tibble::deframe()
}

get_feature_name <- function(id){
    query <- paste(
        "SELECT display FROM features WHERE id = ",
        id
    )
    query %>%
        dplyr::sql() %>%
        .GlobalEnv$perform_query("get feature name") %>%
        dplyr::pull(display)
}

build_value_tbl <- function(tbl, class_id, response_id){
    subquery1 <- paste(
        "SELECT id FROM features WHERE class_id =",
        class_id
    )
    
    subquery2 <- paste(
        "SELECT sample_id, feature_id, value FROM features_to_samples",
        "WHERE feature_id IN (",
        subquery1,
        ")"
    )
    
    subquery3 <- paste(
        "SELECT sample_id, feature_id as response_id, value AS response_value",
        "FROM features_to_samples",
        "WHERE feature_id =",
        response_id
    )
    
    query <- paste(
        "SELECT a.sample_id, a.value, b.response_value, c.display as feature, c.order FROM",
        "(", subquery2, ") a",
        "INNER JOIN",
        "(", subquery3, ") b",
        "ON a.sample_id = b.sample_id",
        "AND a.feature_id <> b.response_id",
        "INNER JOIN features c",
        "ON a.feature_id = c.id"
    )
    
    query %>%
        dplyr::sql() %>%
        .GlobalEnv$perform_query("build value tbl") %>% 
        dplyr::inner_join(tbl, by = "sample_id")
        
}

build_heatmap_matrix <- function(tbl, method){
    tbl %>%  
        dplyr::group_by(group, feature, order) %>%
        dplyr::summarise(value = cor(
            value,
            response_value,
            method = method
        )) %>%
        dplyr::arrange(dplyr::desc(order)) %>% 
        dplyr::select(-order) %>% 
        dplyr::filter_all(dplyr::all_vars(!is.na(.))) %>% 
        tidyr::pivot_wider(names_from = group, values_from = value) %>%
        tibble::column_to_rownames("feature") %>%
        as.matrix()
}

build_scatterplot_tbl <- function(tbl, clicked_feature, clicked_group){
    tbl %>%
        dplyr::filter(feature == clicked_feature, group == clicked_group) %>%
        dplyr::select(group, y = response_value, x = value, name = sample_name) %>%
        create_plotly_label(
            name_column = "name",
            group_column = "group",
            value_columns = c("x", "y")
        )
}