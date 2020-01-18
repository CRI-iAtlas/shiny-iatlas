til_maps_server <- function(
    input, 
    output, 
    session,
    features_con,
    feature_values_con,
    sample_con,
    groups_con,
    group_name,
    cohort_colors
){
    
    tilmap_feature_con <- reactive({
        req(features_con())
        features_con() %>%  
            dplyr::filter(class_name == "TIL Map Characteristic") %>% 
            dplyr::select(feature_name, feature_id, class_name) %>% 
            dplyr::compute()
    })
    
    tilmap_con <- reactive({
        req(tilmap_feature_con(), feature_values_con())
        tilmap_feature_con() %>% 
            dplyr::select(feature_id, feature_name) %>% 
            dplyr::inner_join(feature_values_con(), by = "feature_id") %>% 
            dplyr::filter_all(dplyr::all_vars(!is.na(.))) %>% 
            dplyr::compute()
    })
    

    tilmap_dist_feature_con <- reactive({
        req(tilmap_feature_con())
        tilmap_feature_con() %>%  
            dplyr::compute()
    })
    
    tilmap_dist_value_con <- reactive({
        req(tilmap_con())
        tilmap_con() %>% 
            dplyr::select(feature_id, sample_id, value) %>% 
            dplyr::compute()
    })
    

    callModule(
        distributions_plot_server,
        "dist",
        "tilmap_dist_plot",
        tilmap_dist_value_con,
        tilmap_dist_feature_con,
        sample_con,
        groups_con,
        group_name,
        cohort_colors,
        key_col = "label"
    )
    
    tilmap_tbl <- reactive({
        req(tilmap_con())
        
        tilmap_con() %>% 
            dplyr::mutate(value = round(value, digits = 1)) %>%
            dplyr::select(
                # Link = link,
                Sample = name,
                `Selected Group` = group,
                feature_name,
                value
            ) %>%
            dplyr::collect() %>%
            tidyr::pivot_wider(names_from = feature_name, values_from = value)
    })

    
    callModule(data_table_server, "til_table", tilmap_tbl, escape = F)
    
    
    
}