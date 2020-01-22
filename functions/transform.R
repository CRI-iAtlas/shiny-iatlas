
# volcano plot module ---------------------------------------------------------

create_volcano_drilldown_plot_title <- function(
    volcano_plot_tbl, 
    label_value,
    numerator = "Wt",
    denominator = "Mut"
){
    tbl <- volcano_plot_tbl %>% 
        dplyr::filter(label == label_value) %>% 
        dplyr::as_tibble() 
    p_value  <- round(dplyr::pull(tbl, p_value), 4)
    fc      <- round(dplyr::pull(tbl, fold_change), 4)
    if(fc >= 1){
        fc_string   <- stringr::str_c(numerator, "/", denominator)
    } else {
        fc  <- round(1/fc, 4)
        fc_string <- stringr::str_c(denominator,  "/", numerator)
    }
    title <- stringr::str_c(
        "Cohort:", label_value, "; P = ", p_value, ";", fc_string, ":", fc, sep = " "
    )
}


# immunefeatures --------------------------------------------------------------

build_immune_feature_heatmap_response_con <- function(
    feature_values_con,
    features_con,
    response_feature_id
){
    feature_values_con %>% 
        dplyr::filter(feature_id == response_feature_id) %>%
        dplyr::select(feature_id, sample_id, value, group, sample_name) %>%
        dplyr::filter_all(dplyr::all_vars(!is.na(.))) %>% 
        dplyr::inner_join(features_con, by = "feature_id") %>% 
        dplyr::select(sample_id, sample_name, value1 = value, feature1 = feature_name, group) %>% 
        dplyr::compute()
}

build_immune_feature_heatmap_feature_con <- function(
    feature_values_con,
    features_con,
    feature_ids
){
    feature_values_con %>% 
        dplyr::filter(feature_id %in% feature_ids) %>%
        dplyr::select(feature_id, sample_id, value) %>%
        dplyr::filter_all(dplyr::all_vars(!is.na(.))) %>% 
        dplyr::inner_join(features_con, by = "feature_id") %>% 
        dplyr::select(sample_id, value2 = value, feature2 = feature_name, order) %>% 
        dplyr::compute()
}

build_immune_feature_heatmap_tbl <- function(response_con, feature_con){
    tbl <-
        dplyr::inner_join(response_con, feature_con, by = "sample_id") %>%
        dplyr::arrange(order) %>% 
        dplyr::select(sample_name, feature = feature2, value1, value2, order, group) %>% 
        dplyr::collect()
}


build_immune_feature_heatmap_matrix <- function(tbl, method){
    tbl %>%  
        dplyr::group_by(group, feature, order) %>%
        dplyr::summarise(value = cor(
            value1,
            value2,
            method = method
        )) %>%
        dplyr::arrange(dplyr::desc(order)) %>% 
        dplyr::select(-order) %>% 
        dplyr::filter_all(dplyr::all_vars(!is.na(.))) %>% 
        tidyr::pivot_wider(names_from = group, values_from = value) %>%
        tibble::column_to_rownames("feature") %>%
        as.matrix()
}

build_immune_feature_scatterplot_tbl <- function(tbl, clicked_feature, clicked_group){
    tbl %>%
        dplyr::filter(feature == clicked_feature, group == clicked_group) %>%
        dplyr::select(group, y = value1, x = value2, name = sample_name) %>%
        create_plotly_label(
            name_column = "name",
            group_column = "group",
            value_columns = c("x", "y")
        )
}




# scale db connection functions -----------------------------------------------

scale_db_connection <- function(con, scale_method = "none"){
    if(scale_method %in% c("Log2", "Log2 + 1", "Log10 + 1", "Log10")){
        add_amt <- 0
        base    <- 10
        if(scale_method %in% c("Log2", "Log2 + 1")){
            base <- 2
        }
        if(scale_method %in% c("Log10 + 1", "Log2 + 1")){
            add_amt <- 1
        }
        con <- log_db_connection(con, base, add_amt)
    } else if (scale_method == "None"){
        con <- con
    } else {
        stop("scale method does not exist")
    }
    return(con)
}

log_db_connection <- function(con, base = 10, add_amt = 0){
    con %>% 
        dplyr::mutate(value = value + add_amt) %>%
        dplyr::filter(value > 0) %>% 
        dplyr::mutate(value = log(value, base))
}


# misc ------------------------------------------------------------------------

create_plotly_label <- function(
    df,
    value_columns,
    title = "ParticipantBarcode",
    name_column = "name",
    group_column = "group") {
    
    result_df <- wrapr::let(
        alias = c(
            namevar = name_column,
            groupvar = group_column),
        df %>%
            dplyr::mutate(
                label = stringr::str_glue(
                    "<b>{title}:</b> {name} ({group})",
                    title = title,
                    name = namevar,
                    group = groupvar
                )) %>% 
            tidyr::gather(value_name, value, dplyr::one_of(value_columns)) %>%
            dplyr::mutate(
                value_label = stringr::str_glue(
                    "{name}: {value}",
                    name = stringr::str_to_upper(value_name),
                    value = sprintf("%0.3f", value)
                )
            ) %>%
            dplyr::group_by(label) %>%
            dplyr::mutate(value_label = stringr::str_c(value_label, collapse = "</br>")) %>%
            dplyr::ungroup() %>%
            tidyr::spread(value_name, value) %>%
            tidyr::unite(label, label, value_label, sep = "</br></br>")
    )
    assert_data_has_columns(result_df, c("label", name_column, group_column, value_columns))
    assert_data_has_rows(result_df)
    return(result_df)
    
}