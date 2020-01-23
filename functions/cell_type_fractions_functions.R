build_plot_tbl <- function(class_name){
    subquery1 <- paste0(
        "SELECT id FROM classes WHERE name = '",
        class_name,
        "'"
    ) 
    
    subquery2 <- paste(
        'SELECT id AS feature_id, name as feature_name, "order" FROM',
        'features WHERE class_id = (',
        subquery1,
        ')'
    ) 
    
    subquery3 <- paste(
        "SELECT sample_id, feature_id, value as feature_value",
        "FROM features_to_samples"
    ) 
    
    query <- paste(
        "SELECT a.sample_id, a.feature_id, a.feature_value,",
        "b.feature_name, b.order FROM (", subquery3, ") a",
        "INNER JOIN (", subquery2, ") b",
        "ON a.feature_id = b.feature_id"
    ) 
    
    query %>%
        dplyr::sql() %>% 
        .GlobalEnv$perform_query("build feature table") %>% 
        dplyr::inner_join(sample_tbl(), by = "sample_id") %>% 
        dplyr::select(-sample_id) %>% 
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