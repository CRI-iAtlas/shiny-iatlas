create_cohort_oject <- function(
    sample_ids,
    group_choice,
    dataset,
    groups,
    driver_gene_id = NULL,
    immune_feature_bin_id = NULL,
    immune_feature_bin_number = NULL
){
    if (group_choice %in% c("Immune Subtype", "TCGA Subtype", "TCGA Study")){
        cohort_oject <- create_tag_cohort_oject(sample_ids, group_choice)
    } else if (group_choice == "Driver Mutation") {
        cohort_oject <- create_mutation_cohort_oject(
            sample_ids, 
            group_choice,
            driver_gene_id
        )
    } else if (group_choice() == "Immune Feature Bins"){
        cohort_oject <- create_mutation_cohort_object(
            sample_ids, 
            group_choice,
            immune_feature_bin_id,
            immune_feature_bin_number
        )
    }
    c(
        cohort_oject,
        "dataset"     = dataset,
        "groups"      = groups
    )
}

create_gene_mutation_list <- function(){
    gene_id_query <- paste(
        "SELECT DISTINCT gene_id FROM genes_to_samples",
        "WHERE status IS NOT NULL"
    )
    
    query <- paste(
        "SELECT hgnc AS gene_name, id FROM genes",
        "WHERE id IN (",
        gene_id_query,
        ")"
    )
    
    query %>% 
        .GlobalEnv$perform_query("Get mutation genes") %>% 
        tibble::deframe()
}

create_driver_mutation_list <- function(){
    paste(
        "SELECT hgnc, id FROM (",
        .GlobalEnv$create_gene_type_query("driver_mutation"),
        ") a"
    ) %>% 
        .GlobalEnv$perform_query("Get mutation genes") %>% 
        tibble::deframe()
}

# tag choice ------------------------------------------------------------------

create_tag_cohort_oject <- function(sample_ids, group_choice){
    cohort_tbl  <- create_tag_cohort_tbl(sample_ids, group_choice)
    list(
        "sample_tbl"  = create_tag_sample_tbl(cohort_tbl),
        "group_tbl"   = create_tag_group_tbl(cohort_tbl),
        "group_name"  = group_choice,
        "plot_colors" = create_tag_plot_colors(cohort_tbl)
    )
}

create_tag_cohort_tbl <- function(sample_ids, group){
    sample_subquery <- .GlobalEnv$create_sample_id_query(sample_ids)
    group_subquery <- .GlobalEnv$create_parent_group_query(group)
    
    query <- paste(
        "SELECT s.id AS sample_id, s.sample_id AS sample_name,",
        "s.tissue_id AS slide_id, g.name AS group, g.display AS name,",
        "g.characteristics, g.color FROM",
        "(", sample_subquery, ") s",
        "INNER JOIN samples_to_tags a",
        "ON s.id = a.sample_id",
        "INNER JOIN",
        "(", group_subquery, ") g",
        "ON a.tag_id = g.id"
    )
    
    .GlobalEnv$perform_query(query, "build cohort sample table") 
}

create_tag_sample_tbl <- function(cohort_tbl){
    dplyr::select(
        cohort_tbl, 
        sample_id, 
        sample_name, 
        slide_id,
        group
    )
} 

create_tag_group_tbl <- function(cohort_tbl){
    cohort_tbl %>% 
        dplyr::group_by(group, name, characteristics) %>% 
        dplyr::summarise(size = dplyr::n()) %>% 
        dplyr::ungroup() %>% 
        dplyr::arrange(group)
}

create_tag_plot_colors <- function(cohort_tbl){
    cohort_tbl <- cohort_tbl %>% 
        dplyr::select(group, color) %>% 
        dplyr::distinct() %>% 
        dplyr::arrange(group)
    if(any(is.na(cohort_tbl$color))){
        cohort_tbl <- dplyr::mutate(
            cohort_tbl, 
            color = viridisLite::viridis(dplyr::n())
        )
    }
    tibble::deframe(cohort_tbl) 
}

# mutation choice -------------------------------------------------------------
create_mutation_cohort_oject <- function(sample_ids, group_choice, gene_id){
    gene_name   <- .GlobalEnv$get_gene_name(gene_id)
    sample_tbl  <- create_mutation_sample_tbl(sample_ids, gene_id)
    list(
        "sample_tbl"  = sample_tbl, 
        "group_tbl"   = create_mutation_group_tbl(sample_tbl, gene_name), 
        "group_name"  = paste("Mutation Status:", gene_name),
        "plot_colors" = create_mutation_plot_colors(sample_tbl)
    )
}

create_mutation_sample_tbl <- function(sample_ids, gene_id){
    sample_subquery <- .GlobalEnv$create_sample_id_query(sample_ids)
    
    gene_subquery <- paste(
        "SELECT DISTINCT sample_id, status as group FROM genes_to_samples ",
        "WHERE status IS NOT NULL AND gene_id =",
        gene_id
    )
    
    query <- paste(
        "SELECT s.id AS sample_id, s.sample_id AS sample_name,",
        "s.tissue_id AS slide_id, g.group FROM",
        "(", sample_subquery, ") s",
        "INNER JOIN", 
        "(", gene_subquery, ") g",
        "ON s.id = g.sample_id"
    )
    
    .GlobalEnv$perform_query(query, "build cohort mutation table") 
}


create_mutation_group_tbl <- function(sample_tbl, gene_name){
    sample_tbl %>% 
        dplyr::group_by(group) %>% 
        dplyr::summarise(size = dplyr::n()) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(name = gene_name, characteristics = "Mutation Status") %>% 
        dplyr::arrange(group)
}

create_mutation_plot_colors <- function(sample_tbl){
    sample_tbl %>% 
        dplyr::select(group) %>% 
        dplyr::distinct() %>% 
        dplyr::mutate(color = viridisLite::viridis(dplyr::n())) %>% 
        dplyr::arrange(group) %>% 
        tibble::deframe() 
}

# immune feature bin choice ---------------------------------------------------

create_mutation_cohort_object <- function(
    sample_ids, 
    group_choice,
    feature_id,
    bin_number
){
    feature_name <- .GlobalEnv$get_feature_name(feature_id)
    sample_tbl <- create_feature_bin_sample_tbl(
        sample_ids,
        feature_id, 
        bin_number
    ) 
    list(
        "sample_tbl"  = sample_tbl, 
        "group_tbl"   = create_feature_bin_group_tbl(sample_tbl, feature_name),
        "group_name"  = paste("Immune Feature Bins:", feature_name),
        "plot_colors" = create_feature_bin_plot_colors(sample_tbl)
    )
}

create_feature_bin_sample_tbl <- function(sample_ids, feature_id, n_bins){
    paste(
        "SELECT a.sample_id, a.value, s.sample_id AS sample_name,",
        "s.tissue_id AS slide_id FROM (",
        .GlobalEnv$create_feature_value_query(feature_id),
        "AND sample_id IN (",
        stringr::str_c(sample_ids, collapse = ", "),
        ")) a",
        "INNER JOIN samples s",
        "ON a.sample_id = s.id"
    ) %>% 
        .GlobalEnv$perform_query("build immune feature bin table") %>% 
        dplyr::mutate(group = as.character(cut(value, n_bins))) %>% 
        dplyr::select(-value)
}

create_feature_bin_group_tbl <- function(sample_tbl, feature_name){
    sample_tbl %>% 
        dplyr::group_by(group) %>% 
        dplyr::summarise(size = dplyr::n()) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(
            name = feature_name, 
            characteristics = "Immune feature bin range"
        ) %>% 
        dplyr::arrange(group)
}

create_feature_bin_plot_colors <- function(sample_tbl){
    sample_tbl %>% 
        dplyr::select(group) %>% 
        dplyr::distinct() %>% 
        dplyr::mutate(color = viridisLite::viridis(dplyr::n())) %>% 
        dplyr::arrange(group) %>% 
        tibble::deframe() 
}
