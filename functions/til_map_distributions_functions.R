get_til_map_named_list <- function(){
    
    subquery <- "SELECT id FROM classes WHERE name = 'TIL Map Characteristic'"
    
    query <- paste(
        "SELECT display, id AS feature FROM features",
        "WHERE class_id = (",
        subquery,
        ")"
    )
    
    query %>%
        dplyr::sql() %>% 
        .GlobalEnv$perform_query("build feature table") %>% 
        tibble::deframe()
}

get_leukocyte_fraction_id <- function(){
    query <- "SELECT id FROM features WHERE display = 'Leukocyte Fraction'"
    query %>%
        dplyr::sql() %>% 
        .GlobalEnv$perform_query("get feature id") %>% 
        dplyr::pull(id)
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

build_distplot_tbl <- function(tbl, id, scale_method){
    
    query <- paste(
        "SELECT sample_id, feature_id, value",
        "FROM features_to_samples",
        "WHERE feature_id =",
        id
    ) 
    
    query %>%
        dplyr::sql() %>% 
        .GlobalEnv$perform_query("build immune feature table") %>% 
        dplyr::inner_join(tbl, by = "sample_id") %>% 
        dplyr::select(-sample_id, sample_name) %>% 
        scale_db_connection(scale_method) %>% 
        dplyr::rename(x = group, y = value) 
}