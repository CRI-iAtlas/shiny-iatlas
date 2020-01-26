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





