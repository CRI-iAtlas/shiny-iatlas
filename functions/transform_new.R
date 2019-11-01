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
    scale_function = "None"
){
    
    con <- con %>% 
        dplyr::filter(feature == feature_name) %>% 
        dplyr::select(x, y, label) %>% 
        dplyr::filter_all(dplyr::all_vars(!is.na(.)))
    if(scale_function != "None"){
        add_amt <- 0
        base    <- 10
        if(scale_function %in% c("Log2", "Log2 + 1")){
            base <- 2
        }
        if(scale_function %in% c("Log10 + 1", "Log2 + 1")){
            add_amt <- 1
        }
        con <- con %>% 
            log_db_connection("y", base, add_amt) %>% 
            dplyr::rename(y = value)
            
    }
    return(con)
}

# scale db connection functions -----------------------------------------------

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