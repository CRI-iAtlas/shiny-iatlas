build_results_tbl <- function(group_name, feature_id, min_wt, min_mut){
    
    subquery1 <- paste0(
        "SELECT id from tags WHERE display = '",
        group_name,
        "'"
    )
    
    subquery2 <- paste(
        "SELECT tag_id from tags_to_tags WHERE related_tag_id IN (",
        subquery1,
        ")"
    ) 
    
    subquery3 <- paste(
        "SELECT p_value, fold_change, log10_p_value,",
        "log10_fold_change, gene_id, tag_id",
        "FROM driver_results",
        "WHERE feature_id = ", feature_id,
        "AND tag_id IN (", subquery2, ")",
        "AND n_wt >= ", min_wt,
        "AND n_mut >= ", min_mut
    ) 
    
    paste(
        "SELECT a.p_value, a.fold_change, a.log10_p_value,",
        "a.log10_fold_change, g.gene, g.gene_id, t.group, t.tag_id FROM",
        "(", subquery3, ") a",
        "LEFT OUTER JOIN (SELECT id AS gene_id, hgnc AS gene FROM genes) g",
        "ON a.gene_id = g.gene_id",
        "LEFT OUTER JOIN (SELECT id AS tag_id, name As group FROM tags) t",
        "ON a.tag_id = t.tag_id"
    ) %>% 
        dplyr::sql() %>%
        .GlobalEnv$perform_query("build driver results table") %>% 
        dplyr::mutate(
            gene = dplyr::if_else(
                is.na(gene),
                "missing",
                gene
            ),
            row_n = 1:dplyr::n(),
            label = paste0(gene, ":", group)
        ) 
} 

build_violin_tbl <- function(feature_id, gene_id, tag_id){
    subquery1 <- paste(
        "SELECT sample_id FROM samples_to_tags",
        "WHERE tag_id = ",
        tag_id
    ) 
    
    subquery2 <- paste(
        "SELECT sample_id, value FROM features_to_samples",
        "WHERE feature_id = ", feature_id,
        "AND sample_id IN (", subquery1, ")"
    ) 
    
    subquery3 <- paste(
        "SELECT sample_id, status FROM genes_to_samples",
        "WHERE gene_id  = ", gene_id,
        "AND sample_id IN (", subquery1, ")"
    )
    
    query <- paste(
        "SELECT f.value, g.status FROM",
        "(", subquery2, ") f",
        "INNER JOIN",
        "(", subquery3, ") g",
        "ON f.sample_id = g.sample_id"
    ) %>% 
        dplyr::sql() %>%
        .GlobalEnv$perform_query("build univariate driver violin table") %>% 
        dplyr::select(x = status, y = value)
}








    