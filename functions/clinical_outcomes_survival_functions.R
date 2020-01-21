build_survival_tbl <- function(
    features_con,
    values_con,
    time_feature
){
    if (time_feature == "OS Time") {
        status_feature <- "OS"
    } else if (time_feature == "PFI Time"){
        status_feature <- "PFI"
    } else {
        stop("input$time_feature_choice is not a valid choice")
    }
    tbl <- features_con %>% 
        dplyr::filter(feature_name %in% c(time_feature, status_feature)) %>% 
        dplyr::inner_join(values_con, by = "feature_id") %>% 
        dplyr::select(group, sample_id, feature_name, value) %>% 
        dplyr::collect() %>% 
        tidyr::pivot_wider(values_from = value, names_from = feature_name) %>%
        dplyr::select(
            group,
            time = time_feature,
            status = status_feature
        ) %>% 
        dplyr::filter_all(dplyr::all_vars(!is.na(.)))
}