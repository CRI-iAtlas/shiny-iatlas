# survival module -------------------------------------------------------------

build_survival_tbl <- function(
    con,
    group_list, 
    group_feature, 
    time_feature, 
    n_groups
){

    if (time_feature == "OS_time") {
        status_feature <- "OS"
    } else {
        status_feature <- "PFI_1"
    }

    if (group_feature %in% group_list){
        tbl <- con %>% 
            dplyr::filter(feature %in% c(time_feature, status_feature)) %>% 
            dplyr::as_tibble() %>% 
            tidyr::pivot_wider(values_from = value, names_from = feature) %>% 
            dplyr::select(
                group,
                time = time_feature,
                status = status_feature
            ) %>% 
            dplyr::filter_all(dplyr::all_vars(!is.na(.)))
    } else {
        tbl <- con %>% 
            dplyr::filter(feature %in% c(time_feature, status_feature, group_feature)) %>% 
            dplyr::as_tibble() %>% 
            tidyr::pivot_wider(values_from = value, names_from = feature) %>% 
            dplyr::filter_all(dplyr::all_vars(!is.na(.))) %>% 
            dplyr::select(
                group = group_feature,
                time = time_feature,
                status = status_feature
            ) %>% 
            dplyr::mutate(group = cut(group, n_groups, ordered_result = T))
    }
    return(tbl)
}

build_ci_matrix <- function(
    con, features_con, features, time_feature, status_feature
){
    features_tbl <- features_con %>% 
        dplyr::filter(feature %in% features) %>% 
        dplyr::arrange(order) %>% 
        dplyr::select(feature, display) %>% 
        dplyr::as_tibble()
    
    con %>% 
        dplyr::filter(feature %in% c(features, time_feature, status_feature)) %>% 
        dplyr::as_tibble() %>% 
        tidyr::pivot_wider(c(sample, group), names_from = feature, values_from = value) %>% 
        tidyr::pivot_longer(features) %>% 
        dplyr::select(-sample) %>% 
        tidyr::nest(value = value, data = c(time_feature, status_feature)) %>% 
        dplyr::mutate(
            value = purrr::map(value, as.matrix),
            data = purrr::map(data, as.matrix)
        ) %>% 
        dplyr::mutate(result = purrr::map2_dbl(value, data, concordanceIndex::concordanceIndex)) %>% 
        dplyr::select(-c(value, data)) %>% 
        tidyr::pivot_wider(name, names_from = group, values_from = result) %>% 
        dplyr::inner_join(features_tbl, by = c("name" = "feature")) %>% 
        dplyr::select(-name) %>% 
        tidyr::drop_na() %>% 
        as.data.frame() %>% 
        tibble::column_to_rownames("display") %>% 
        as.matrix()
}

# cell fractions module -------------------------------------------------------

build_cell_fractions_barplot_tbl <- function(feature_con, value_con){
    value_con %>% 
        dplyr::inner_join(feature_con, by = "feature") %>% 
        dplyr::group_by(display, group) %>% 
        dplyr::arrange(order) %>% 
        dplyr::summarise(mean = mean(value), count = dplyr::n()) %>%
        dplyr::ungroup() %>% 
        dplyr::mutate(se = mean / sqrt(count)) %>% 
        dplyr::as_tibble() %>% 
        create_plotly_label(
            name_column = "display",
            group_column = "group",
            value_columns = c("mean", "se")
        ) %>% 
        dplyr::select(x = group, y = mean, color = display, label, error = se)
}

 
# overall cell proportions module ---------------------------------------------

build_cell_proportion_con <- function(feature_con, value_con){

    feature_con <- feature_con %>% 
        dplyr::filter(class == "Overall Proportion") %>% 
        dplyr::filter(feature != "til_percentage")
    
    value_con %>% 
        dplyr::inner_join(feature_con) 
        
}

build_cell_proportion_barplot_tbl <- function(con){
    con %>%
        dplyr::group_by(display, group) %>% 
        dplyr::summarise(mean = mean(value), count = dplyr::n()) %>%
        dplyr::mutate(se = mean / sqrt(count)) %>%
        dplyr::as_tibble() %>%
        create_plotly_label(
            title = "Fraction",
            name_column = "display",
            group_column = "group",
            value_columns = c("mean", "se")
        ) %>%
        dplyr::select(label, color = display, x = group, y = mean, error = se)
}

build_cell_proportion_scatterplot_tbl <- function(con){
    con %>% 
        dplyr::select(sample, group, feature = display, value) %>% 
        dplyr::filter(
            feature %in% c("Leukocyte Fraction", "Stromal Fraction")
        ) %>%
        dplyr::as_tibble() %>% 
        tidyr::pivot_wider(values_from = value, names_from = feature) %>% 
        tidyr::drop_na() %>% 
        dplyr::rename(x = `Stromal Fraction`, y = `Leukocyte Fraction`) %>% 
        create_plotly_label(
            name_column = "sample",
            group_column = "group",
            value_columns = c("x", "y")
        ) %>% 
        dplyr::select(x, y, label)
}


# volcano plot module ---------------------------------------------------------

create_volcano_drilldown_plot_title <- function(
    volcano_plot_con, 
    label_value,
    numerator = "Wt",
    denominator = "Mut"
){
    tbl <- volcano_plot_con %>% 
        dplyr::filter(label == label_value) %>% 
        dplyr::as_tibble()
    feature <- get_unique_values_from_column(tbl, "feature")
    pvalue  <- round(dplyr::pull(tbl, pvalue), 4)
    fc      <- round(dplyr::pull(tbl, fold_change), 4)
    if(fc >= 1){
        fc_string   <- stringr::str_c(numerator, "/", denominator)
    } else {
        fc  <- round(1/fc, 4)
        fc_string <- stringr::str_c(denominator,  "/", numerator)
    }
    title <- stringr::str_c(
        "Cohort:", label_value, "; P = ", pvalue, ";", fc_string, ":", fc, sep = " "
    )
}

# server ----------------------------------------------------------------------

subset_long_con_with_group <- function(
    con, 
    user_group_tbl,
    group_col, 
    group_values = "none",
    feature_col = "feature",
    value_col = "value"
){
    if(is.data.frame(user_group_tbl) & !(group_col %in% colnames(con))){
        # needs to be a general solution to any backend
        
        # DBI::dbCreateTable(PANIMMUNE_DB, "user_groups", user_group_tbl)
        # DBI::dbAppendTable(PANIMMUNE_DB, "user_groups", user_group_tbl)
        # user_groups_con <- dplyr::tbl(PANIMMUNE_DB, "user_groups")
        # 
        # con <- con %>% 
        #     dplyr::inner_join(user_groups_con, by = "sample") %>% 
        #     dplyr::select("sample", "group" = group_col, feature_col, value_col) %>% 
        #     dplyr::filter_all(dplyr::all_vars(!is.na(.)))
        
    } else {
        con <- con %>% 
            dplyr::select("sample", "group" = group_col, feature_col, value_col) %>% 
            dplyr::filter_all(dplyr::all_vars(!is.na(.)))
    }
    
    if(group_values[[1]] != "none"){
        con <- dplyr::filter(con, group %in% group_values)
    }
    return(con)
}


# immunefeatures --------------------------------------------------------------


build_immune_feature_heatmap_con <- function(
    feature_value_con,
    feature_con,
    feature_choices,
    response_feature
){
    response_con <- build_immune_feature_heatmap_response_con(
        feature_value_con,
        response_feature
    )

    feature_choices_con <- build_immune_feature_heatmap_choices_con(
        feature_value_con,
        feature_choices
    )

    heatmap_con <-
        dplyr::inner_join(response_con, feature_choices_con, by = "sample") %>%
        dplyr::inner_join(feature_con, by = c("feature")) %>%
        dplyr::select(sample, group, feature = display, value1, value2)
}

build_immune_feature_heatmap_response_con <- function(con, response_feature){
    con %>%
        dplyr::filter(feature == response_feature) %>%
        dplyr::select(sample, group, value1 = value) %>%
        dplyr::filter_all(dplyr::all_vars(!is.na(.)))
}

build_immune_feature_heatmap_choices_con <- function(con, features){
    con %>%
        dplyr::select(sample, feature, value2 = value) %>%
        dplyr::filter(feature %in% features) %>%
        dplyr::filter_all(dplyr::all_vars(!is.na(.)))
}

build_immune_feature_heatmap_matrix <- function(con, method){
    con %>%
        dplyr::select(-sample) %>%
        dplyr::as_tibble() %>%
        dplyr::group_by(group, feature) %>%
        dplyr::summarise(value = cor(
            value1,
            value2,
            method = method
        )) %>%
        tidyr::drop_na() %>%
        tidyr::pivot_wider(names_from = group, values_from = value) %>%
        tibble::column_to_rownames("feature") %>%
        as.matrix()
}

build_immune_feature_scatterplot_tbl <- function(con, clicked_feature){
    con %>%
        dplyr::filter(feature == clicked_feature) %>%
        dplyr::select(sample, group, y = value1, x = value2) %>%
        dplyr::as_tibble() %>%
        create_plotly_label(
            name_column = "sample",
            group_column = "group",
            value_columns = c("x", "y")
        )
}


# distribution plot functions -------------------------------------------------

build_distribution_plot_tbl <- function(
    con, 
    feature_name, 
    scale_method = "None"
){
    con %>% 
        dplyr::filter(feature == feature_name) %>% 
        dplyr::select(x, y, label) %>% 
        dplyr::filter_all(dplyr::all_vars(!is.na(.))) %>% 
        scale_db_connection(scale_method)
}

# scale db connection functions -----------------------------------------------

scale_db_connection <- function(con, scale_method = "none", col = "value"){
    if(scale_method %in% c("Log2", "Log2 + 1", "Log10 + 1", "Log10")){
        add_amt <- 0
        base    <- 10
        if(scale_method %in% c("Log2", "Log2 + 1")){
            base <- 2
        }
        if(scale_method %in% c("Log10 + 1", "Log2 + 1")){
            add_amt <- 1
        }
        con <- con %>% 
            log_db_connection("y", base, add_amt) %>% 
            dplyr::rename(y = value)
    } else if (scale_method == "None"){
        con <- con
    } else {
        stop("scale method does not exist")
    }
    return(con)
}

log_db_connection <- function(con, col = "value", base = 10, add_amt = 0){
    con %>% 
        dplyr::select(value = col, dplyr::everything()) %>%
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
            ungroup() %>%
            tidyr::spread(value_name, value) %>%
            tidyr::unite(label, label, value_label, sep = "</br></br>")
    )
    assert_df_has_columns(result_df, c("label", name_column, group_column, value_columns))
    assert_df_has_rows(result_df)
    return(result_df)
    
}