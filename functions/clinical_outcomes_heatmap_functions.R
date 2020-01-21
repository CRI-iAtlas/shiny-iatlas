get_time_feature_id <- function(features_con, name){
    features_con %>% 
        dplyr::filter(feature_name == name) %>% 
        dplyr::pull(feature_id)
}

get_status_feature_id <- function(features_con, time_feature){
    if(time_feature == "OS Time") {
        status_feature <- "OS"
    } else if (time_feature == "PFI Time"){
        status_feature <- "PFI"
    } else {
        stop("time_feature is not a valid choice")
    }
    features_con %>% 
        dplyr::filter(feature_name == status_feature) %>% 
        dplyr::pull(feature_id)
}

get_feature_ids <- function(features_con, class_choice_id){
    features_con %>% 
        dplyr::filter(class_id == class_choice_id) %>% 
        dplyr::arrange(order) %>% 
        dplyr::pull(feature_id)
}

build_feature_values_con <- function(
    feature_values_con,
    features_con,
    feature_ids,
    sample_tbl
){
 
    con <- feature_values_con %>%
        dplyr::filter(feature_id %in% feature_ids) %>%
        dplyr::inner_join(features_con, by = "feature_id") %>%
        dplyr::select(sample_id, feature_name, value) %>%
        dplyr::compute()

    # sample_id_sting <- sample_tbl %>% 
    #     dplyr::pull(sample_id) %>% 
    #     stringr::str_c(collapse = ", ")
    # feature_id_string <- stringr::str_c(feature_ids, collapse = ", ")
    # 
    # query <- dplyr::sql(stringr::str_c(
    #     'SELECT sample_id, feature_id, value',
    #     'FROM features_to_samples',
    #     'WHERE value IS NOT NULL',
    #     'AND sample_id IN (',
    #     sample_id_sting,
    #     ')',
    #     'AND feature_id IN (',
    #     feature_id_string,
    #     ')',
    #     sep = " "
    # ))
    # 

    return(con)
}

build_survival_values_con <- function(
    feature_values_con,
    features_con,
    status_feature_id,
    time_feature_id
){

    time_con <- feature_values_con %>% 
        dplyr::filter(feature_id == time_feature_id) %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::inner_join(features_con, by = "feature_id") %>%
        dplyr::select(sample_id, group, time = value)
    
    status_con <- feature_values_con %>% 
        dplyr::filter(feature_id == status_feature_id) %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::inner_join(features_con, by = "feature_id") %>%
        dplyr::select(sample_id, status = value)
    
    con <-
        dplyr::inner_join(
            time_con,
            status_con,
            by = "sample_id"
        ) %>%  
        dplyr::select(sample_id, time, status, group) %>% 
        dplyr::compute() 

}

build_ci_matrix <- function(feature_values_con, survival_values_con){
    mat <-
        dplyr::inner_join(
            feature_values_con, 
            survival_values_con,
            by = "sample_id"
        ) %>% 
        dplyr::select(feature_name, value, time, status, group) %>% 
        dplyr::collect() %>% 
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
        dplyr::select(feature_name, group, result) %>% 
        tidyr::pivot_wider(
            feature_name,
            names_from = group,
            values_from = result
        ) %>% 
        as.data.frame() %>% 
        tibble::column_to_rownames("feature_name") %>% 
        as.matrix()
}