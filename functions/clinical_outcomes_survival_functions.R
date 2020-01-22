get_status_feature_name <- function(time_feature){
    if (time_feature == "OS Time") {
        status_feature <- "OS"
    } else if (time_feature == "PFI Time"){
        status_feature <- "PFI"
    } else {
        stop("input$time_feature_choice is not a valid choice")
    }
    return(status_feature)
}

build_value_tbl <- function(sample_tbl, time_feature, status_feature){
    subquery1 <- paste(
        'SELECT id AS feature_id, display AS feature_name',
        'FROM features WHERE display IN(',
        stringr::str_c(
            "'",
            c(time_feature, status_feature),
            "'",
            collapse = ", "
        ),
        ")"
    ) 
    
    subquery2 <- paste(
        "SELECT sample_id, feature_id, value as feature_value",
        "FROM features_to_samples"
    )
    
    query <- paste(
        "SELECT a.feature_name, b.sample_id, b.feature_value FROM",
        "(", subquery1, ") a",
        "INNER JOIN",
        "(", subquery2, ") b", 
        "ON a.feature_id = b.feature_id" 
    )
    
    query %>% 
        dplyr::sql() %>% 
        .GlobalEnv$perform_query(
            "build clinical survial outcomes value table"
        ) %>% 
        dplyr::inner_join(sample_tbl, by = "sample_id") %>% 
        dplyr::select(group, sample_id, feature_name, feature_value) %>% 
        tidyr::pivot_wider(values_from = feature_value, names_from = feature_name) %>%
        dplyr::select(
            group,
            time = time_feature,
            status = status_feature
        ) %>% 
        dplyr::filter_all(dplyr::all_vars(!is.na(.)))
}
