feature_con_has_multiple_group_cols <- function(con){
    .GlobalEnv$assert_data_has_columns(con, c("feature_id", "feature_name"))
    num_group_columns <- con %>%
        dplyr::select(-c(feature_id, feature_name)) %>% 
        colnames() %>% 
        length() 
    return(num_group_columns > 1)
}

get_feature_group_names <- function(con){
    .GlobalEnv$assert_data_has_columns(con, c("feature_id", "feature_name"))
    con %>% 
        dplyr::select(-c(feature_id, feature_name)) %>% 
        colnames()
}