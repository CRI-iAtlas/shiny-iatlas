create_gene_mutation_list <- function(){
    id_query <- dplyr::sql(paste(
        "SELECT DISTINCT gene_id FROM genes_to_samples",
        "WHERE status IS NOT NULL"
    ))
    
    gene_id_string <- id_query %>% 
        .GlobalEnv$perform_query("get_gene_mutation_ids") %>% 
        dplyr::pull(gene_id) %>% 
        stringr::str_c(collapse = ", ")
    
    list_query <- dplyr::sql(paste(
        "SELECT hgnc AS gene_name, id FROM genes",
        "WHERE id IN (",
        gene_id_string,
        ")"
    ))
    
    gene_list <- list_query %>% 
        .GlobalEnv$perform_query("get_gene_tbl") %>% 
        tibble::deframe()
}

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

create_group_tbl1 <- function(group){
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
    
    tag_ids_string <- tag_id_query %>% 
        .GlobalEnv$perform_query("get_tag_ids") %>% 
        dplyr::pull(tag_id) %>% 
        stringr::str_c(collapse = ", ")
    
    group_tbl_query <- dplyr::sql(paste(
        "SELECT id as tag_id, name as group, display as name, characteristics, color",
        "FROM tags WHERE id IN (", 
        tag_ids_string,
        ")"
    )) 
    
    group_tbl <- group_tbl_query %>% 
        .GlobalEnv$perform_query("create_tag_tbl") 
}

create_sample_tbl1 <- function(sample_ids){
    subquery <- paste(
        "SELECT id as sample_id, sample_id as sample_name FROM samples where id IN (",
        stringr::str_c(sample_ids, collapse = ", "),
        ")"
    ) 
    
    query <- dplyr::sql(paste(
        "SELECT a.sample_id, a.sample_name, b.tag_id FROM",
        "(", subquery, ") a",
        "INNER JOIN", 
        "(SELECT * FROM samples_to_tags) b",
        "ON a.sample_id = b.sample_id"
    )) 
    
    query %>% 
        .GlobalEnv$perform_query("create_sample_tbl")
}

create_sample_tbl2 <- function(sample_ids, gene_id){
    
    subquery <- paste0(
        "SELECT DISTINCT sample_id, status as group FROM genes_to_samples ",
        "WHERE status IS NOT NULL ",
        "AND gene_id = '", gene_id, "'"
    )
    
    query <- dplyr::sql(paste(
        "SELECT a.sample_id, a.sample_name, b.group FROM",
        "(SELECT id as sample_id, sample_id as sample_name FROM samples) a",
        "INNER JOIN", 
        "(", subquery, ") b",
        "ON a.sample_id = b.sample_id"
    )) 
    
    tbl <- query %>% 
        .GlobalEnv$perform_query("build_gene_status_tbl") 
}

