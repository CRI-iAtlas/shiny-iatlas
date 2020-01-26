get_numeric_filter_samples <- function(feature_id, min, max){
    query <- dplyr::sql(paste(
        "SELECT sample_id FROM features_to_samples",
        "WHERE value <=",
        max,
        "AND value >=",
        min,
        "AND feature_id =",
        feature_id, 
        sep = " "
    ))
    query %>% 
        .GlobalEnv$perform_query("get_numeric_filter_samples") %>% 
        dplyr::pull(sample_id)
}

get_group_filter_samples <- function(group_ids){
    sample_id_query <- dplyr::sql(paste0(
        "SELECT sample_id FROM samples_to_tags WHERE tag_id IN (", 
        stringr::str_c(group_ids, collapse = ", "),
        ")" 
    )) %>% 
        .GlobalEnv$perform_query("get sample ids") %>% 
        dplyr::pull(sample_id) 
}