

get_all_dataset_ids <- function(dataset){
    if(dataset == "TCGA"){
        tag_name <- "TCGA Study"
    } else if(dataset == "PCAWG"){
        tag_name <- "PCAWG Study"
    } else {
        tag_name = dataset
    }
    
    parent_tag_query <- dplyr::sql(paste0(
        "SELECT id FROM tags WHERE (display = '", 
        tag_name,
        "')"
    ))
    parent_tag_id <- parent_tag_query %>% 
        .GlobalEnv$perform_query("get_parent_tag_id") %>% 
        dplyr::pull(id) 
    
    tag_id_query <- dplyr::sql(paste0(
        "SELECT tag_id FROM tags_to_tags WHERE (related_tag_id = ", 
        parent_tag_id,
        ")"
    )) 
    tag_id_string <- tag_id_query %>% 
        .GlobalEnv$perform_query("get_tag_ids") %>% 
        dplyr::pull(tag_id) %>% 
        stringr::str_c(collapse = ", ")
    
    sample_id_query <- dplyr::sql(paste0(
        "SELECT sample_id FROM samples_to_tags WHERE tag_id IN (", 
        tag_id_string,
        ")" 
    )) 
    
    sample_ids <- sample_id_query %>% 
        .GlobalEnv$perform_query("get_sample_ids") %>% 
        dplyr::pull(sample_id) 
}

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

get_group_filter_samples <- function(group, values){
    
    parent_tag_query <- dplyr::sql(paste0(
        "SELECT id FROM tags WHERE (display = '", 
        group,
        "')"
    )) 
    
    parent_tag_id <- parent_tag_query %>% 
        .GlobalEnv$perform_query("get_parent_tag_id") %>% 
        dplyr::pull(id) 
    
    tag_id_query <- dplyr::sql(paste0(
        "SELECT tag_id FROM tags_to_tags WHERE (related_tag_id = ", 
        parent_tag_id,
        ")"
    )) 
    
    tag_ids_string1 <- tag_id_query %>% 
        .GlobalEnv$perform_query("get_tag_ids") %>% 
        dplyr::pull(tag_id) %>% 
        stringr::str_c(collapse = ", ")
    
    selected_tag_ids_query <- dplyr::sql(paste0(
        "SELECT id FROM tags WHERE id IN (", 
        tag_ids_string1,
        ") ",
        "AND name IN (",
        stringr::str_c("'", values, "'", collapse = ", "),
        ")"
    )) 
    
    tag_ids_string2 <- selected_tag_ids_query %>% 
        .GlobalEnv$perform_query("get_tag_ids") %>% 
        dplyr::pull(id) %>% 
        stringr::str_c(collapse = ", ")
    
    sample_id_query <- dplyr::sql(paste0(
        "SELECT sample_id FROM samples_to_tags WHERE tag_id IN (", 
        tag_ids_string2,
        ")" 
    )) 
    
    sample_ids <- sample_id_query %>% 
        .GlobalEnv$perform_query("get_sample_ids") %>% 
        dplyr::pull(sample_id) 
}



