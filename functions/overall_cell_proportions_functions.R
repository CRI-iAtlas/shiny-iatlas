build_value_tbl <- function(sample_tbl){
    subquery1 <- paste(
        'SELECT id AS feature_id, "display" AS feature_name, "order"',
        'FROM features WHERE display IN',
        "('Leukocyte Fraction', 'Stromal Fraction', 'Tumor Fraction')"
    )
    
    subquery2 <- paste(
        "SELECT sample_id, feature_id, value as feature_value",
        "FROM features_to_samples"
    )
    
    query <- paste(
        "SELECT a.feature_name, a.order, b.sample_id, b.feature_value FROM",
        "(", subquery1, ") a",
        "INNER JOIN",
        "(", subquery2, ") b", 
        "ON a.feature_id = b.feature_id" 
    )
    
    query %>% 
        dplyr::sql() %>% 
        .GlobalEnv$perform_query(
            "build overall_cell_proportions value table"
        ) %>% 
        dplyr::inner_join(sample_tbl, by = "sample_id") %>% 
        dplyr::select(-sample_id)
}

build_barplot_tbl <- function(value_tbl){
    value_tbl %>%
        dplyr::group_by(feature_name, group) %>% 
        dplyr::arrange(order) %>%
        dplyr::summarise(mean = mean(feature_value), count = dplyr::n()) %>%
        dplyr::ungroup() %>% 
        dplyr::mutate(se = mean / sqrt(count)) %>% 
        .GlobalEnv$create_plotly_label(
            name_column = "feature_name",
            group_column = "group",
            value_columns = c("mean", "se")
        ) %>% 
        dplyr::select(
            x = group, 
            y = mean, 
            color = feature_name, 
            label, 
            error = se
        ) 
}


build_scatterplot_tbl <- function(value_tbl, group_value){
    value_tbl %>%
        dplyr::select(
            sample_name,
            group,
            feature = feature_name,
            value = feature_value) %>%
        dplyr::filter(
            feature %in% c("Leukocyte Fraction", "Stromal Fraction"),
            group == group_value
        ) %>%
        tidyr::pivot_wider(values_from = value, names_from = feature) %>%
        tidyr::drop_na() %>%
        dplyr::rename(x = `Stromal Fraction`, y = `Leukocyte Fraction`) %>%
        .GlobalEnv$create_plotly_label(
            name_column = "sample_name",
            group_column = "group",
            value_columns = c("x", "y")
        ) %>%
        dplyr::select(x, y, label)
}