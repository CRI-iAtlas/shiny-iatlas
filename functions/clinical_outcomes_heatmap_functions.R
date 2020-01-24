get_t_helper_score_class_id <- function(){
    query <- "SELECT * FROM classes WHERE name = 'T Helper Cell Score'"
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

get_feature_id <- function(name){
    query <- paste0(
        "SELECT id FROM features WHERE display = '",
        name,
        "'"
    )
    query %>% 
        dplyr::sql() %>%
        .GlobalEnv$perform_query("get feature id") %>%
        dplyr::pull(id)
}

get_status_feature_name <- function(time_feature){
    if(time_feature == "OS Time") {
        status_feature <- "OS"
    } else if (time_feature == "PFI Time"){
        status_feature <- "PFI"
    } else {
        stop("time_feature is not a valid choice")
    }
    return(status_feature)
}

build_value_tbl <- function(tbl, class_id, time_feature_id, status_feature_id){
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
        "SELECT sample_id, value AS time",
        "FROM features_to_samples",
        "WHERE feature_id =",
        time_feature_id
    )
    
    subquery4 <- paste(
        "SELECT sample_id, value AS status",
        "FROM features_to_samples",
        "WHERE feature_id =",
        status_feature_id
    )
    
    query <- paste(
        "SELECT a.sample_id, a.value, b.time, c.status,",
        "d.display as feature FROM",
        "(", subquery2, ") a",
        "INNER JOIN",
        "(", subquery3, ") b",
        "ON a.sample_id = b.sample_id",
        "INNER JOIN",
        "(", subquery4, ") c",
        "ON a.sample_id = c.sample_id",
        "INNER JOIN features d",
        "ON a.feature_id = d.id"
    )
    
    query %>%
        dplyr::sql() %>%
        .GlobalEnv$perform_query("build value tbl") %>% 
        dplyr::inner_join(tbl, by = "sample_id")
}

build_hetamap_matrix <- function(tbl){
    mat <- tbl %>% 
        dplyr::select(feature, value, time, status, group) %>%
        tidyr::nest(value = value, data = c(time, status)) %>%
        dplyr::mutate(
            value = purrr::map(value, as.matrix),
            data = purrr::map(data, as.matrix)
        ) %>%
        dplyr::mutate(result = purrr::map2_dbl(
            value,
            data,
            concordanceIndex::concordanceIndex
        )) %>%
        dplyr::select(feature, group, result) %>%
        tidyr::pivot_wider(
            feature,
            names_from = group,
            values_from = result
        ) %>%
        as.data.frame() %>%
        tibble::column_to_rownames("feature") %>%
        as.matrix()
}
