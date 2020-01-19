# survival module -------------------------------------------------------------

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

build_ci_matrix <- function(feature_values_con, survival_values_con){

    mat <-
        dplyr::inner_join(
            feature_values_con, 
            survival_values_con,
            by = "sample_id"
        ) %>% 
        dplyr::select(feature_name, value, time, status, group) %>% 
        dplyr::collect() %>% 
        tidyr::nest(value = value, data = c(time, status)) %>% 
        dplyr::mutate(
            value = purrr::map(value, as.matrix),
            data = purrr::map(data, as.matrix)
        ) %>% 
        dplyr::mutate(result = purrr::map2_dbl(value, data, concordanceIndex::concordanceIndex)) %>% 
        dplyr::select(feature_name, group, result) %>% 
        tidyr::pivot_wider(feature_name, names_from = group, values_from = result) %>% 
        as.data.frame() %>% 
        tibble::column_to_rownames("feature_name") %>% 
        as.matrix() 
}

# cell fractions module -------------------------------------------------------

build_cell_fractions_barplot_tbl <- function(feature_tbl, value_tbl, class_value){
    feature_tbl %>% 
        dplyr::filter(class_name == class_value) %>% 
        dplyr::inner_join(value_tbl, by = "feature_id") %>% 
        dplyr::group_by(feature_name, group) %>% 
        dplyr::arrange(order) %>% 
        dplyr::summarise(mean = mean(value), count = dplyr::n()) %>%
        dplyr::ungroup() %>% 
        dplyr::mutate(se = mean / sqrt(count)) %>% 
        dplyr::collect() %>% 
        create_plotly_label(
            name_column = "feature_name",
            group_column = "group",
            value_columns = c("mean", "se")
        ) %>% 
        dplyr::select(x = group, y = mean, color = feature_name, label, error = se) 
}

 
# overall cell proportions module ---------------------------------------------



build_cell_proportion_barplot_tbl <- function(con){
    con %>%
        dplyr::group_by(feature_name, group) %>% 
        dplyr::summarise(mean = mean(value), count = dplyr::n()) %>%
        dplyr::mutate(se = mean / sqrt(count)) %>%
        dplyr::collect() %>% 
        create_plotly_label(
            title = "Fraction",
            name_column = "feature_name",
            group_column = "group",
            value_columns = c("mean", "se")
        ) %>%
        dplyr::select(label, color = feature_name, x = group, y = mean, error = se)
        
}

build_cell_proportion_scatterplot_tbl <- function(con, group_value){
    con %>% 
        dplyr::select(sample_name, group, feature = feature_name, value) %>% 
        dplyr::filter(
            feature %in% c("Leukocyte Fraction", "Stromal Fraction"),
            group == group_value
        ) %>%
        dplyr::collect() %>% 
        tidyr::pivot_wider(values_from = value, names_from = feature) %>% 
        tidyr::drop_na() %>% 
        dplyr::rename(x = `Stromal Fraction`, y = `Leukocyte Fraction`) %>% 
        create_plotly_label(
            name_column = "sample_name",
            group_column = "group",
            value_columns = c("x", "y")
        ) %>% 
        dplyr::select(x, y, label)
}


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