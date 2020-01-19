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
    
    source("modules/server/submodules/data_table_server.R", local = T)
    source("modules/server/submodules/distribution_plot_server.R", local = T)
    
    tilmap_feature_con <- shiny::reactive({
        shiny::req(features_con())
        features_con() %>%  
            dplyr::filter(class_name == "TIL Map Characteristic") %>% 
            dplyr::select(feature_name, feature_id, class_name) %>% 
            dplyr::compute()
    })
    
    tilmap_con <- shiny::reactive({
        shiny::req(tilmap_feature_con(), feature_values_con())
        tilmap_feature_con() %>% 
            dplyr::select(feature_id, feature_name) %>% 
            dplyr::inner_join(feature_values_con(), by = "feature_id") %>% 
            dplyr::filter_all(dplyr::all_vars(!is.na(.))) %>% 
            dplyr::compute()
    })
    

    tilmap_dist_feature_con <- shiny::reactive({
        shiny::req(tilmap_feature_con())
        tilmap_feature_con() %>%  
            dplyr::compute()
    })
    
    tilmap_dist_value_con <- shiny::reactive({
        shiny::req(tilmap_con())
        tilmap_con() %>% 
            dplyr::select(feature_id, sample_id, value, group) %>% 
            dplyr::compute()
    })
    

    shiny::callModule(
        distributions_plot_server,
        "dist",
        plot_source_name           = "tilmap_dist_plot",
        feature_values_con         = tilmap_dist_value_con,
        feature_metadata_con       = tilmap_dist_feature_con,
        groups_con                 = groups_con,
        group_display_choice       = group_name,
        plot_colors                = cohort_colors,
        key_col                    = "label"
    )
    
    tilmap_tbl <- shiny::reactive({
        shiny::req(tilmap_con())
        
        tilmap_con() %>% 
            dplyr::mutate(value = round(value, digits = 1)) %>%
            print() %>% 
            dplyr::select(
                # Link = link,
                Sample = sample_name,
                `Selected Group` = group,
                feature_name,
                value
            ) %>%
            dplyr::collect() %>%
            tidyr::pivot_wider(names_from = feature_name, values_from = value)
    })

    
    shiny::callModule(data_table_server, "til_table", tilmap_tbl, escape = F)
    
    
    
}