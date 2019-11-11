tilmap_UI <- function(id) {
    
    ns <- NS(id)
    
    tagList(
        titleBox("iAtlas Explorer — TIL Maps"),
        textBox(
            width = 12,
            includeMarkdown("data/markdown/tilmap.markdown")
        ),
        
        distributions_plot_module_UI(
            ns("dist"),
            message_html = includeMarkdown("data/markdown/tilmap_dist.markdown"),
            title_text = "TIL Map Characteristics",
            click_text = 
                "Click point or violin/box to filter samples in table below"
        ),

        
        data_table_module_UI(
            ns("til_table"),
            title = "TIL Map Annotations",
            message_html = includeMarkdown("data/markdown/tilmap_table.markdown")
        )
    )
}

tilmap <- function(
    input, 
    output, 
    session, 
    group_display_choice,
    groups_con,
    til_image_links_con,
    feature_values_con,
    features_con,
    plot_colors
){
    
    tilmap_feature_con <- reactive({
        req(features_con())
        features_con() %>%  
            dplyr::filter(class == "TIL Map Characteristic") %>% 
            dplyr::select(display, feature, class) %>% 
            dplyr::filter_all(dplyr::all_vars(!is.na(.)))
    })
    
    tilmap_dist_feature_con <- reactive({
        req(tilmap_feature_con())
        dplyr::rename(
            tilmap_feature_con(), 
            DISPLAY = display, 
            INTERNAL = feature, 
            CLASS = class
        )
    })
    
    tilmap_features <- reactive({
        req(tilmap_feature_con())
        get_unique_values_from_column(tilmap_feature_con(), "feature")
    })
    
    tilmap_value_con <- reactive({
        req(feature_values_con(), tilmap_features())
        dplyr::filter(
            feature_values_con(), 
            feature %in% local(tilmap_features())
        )
    })
    
    tilmap_dist_value_con <- reactive({
        req(tilmap_value_con())
        dplyr::select(
            tilmap_value_con(), 
            label = sample, 
            x = group, 
            feature, 
            y = value
        )
    })
    
    tilmap_tbl <- reactive({
        req(tilmap_value_con(), tilmap_feature_con(), til_image_links_con())
        
        tilmap_value_con() %>% 
            dplyr::inner_join(tilmap_feature_con(), by = "feature") %>% 
            dplyr::select(sample, group, feature, value, display) %>% 
            dplyr::filter_all(dplyr::all_vars(!is.na(.))) %>% 
            dplyr::mutate(value = round(value, digits = 1)) %>%
            dplyr::inner_join(til_image_links_con(), by = "sample") %>% 
            dplyr::select(
                Link = link, 
                Sample = sample, 
                `Selected Group` = group,
                display,
                value
            ) %>% 
            dplyr::as_tibble() %>% 
            tidyr::pivot_wider(names_from = display, values_from = value)
    })
    

    callModule(
        distributions_plot_module,
        "dist",
        "tilmap_dist_plot",
        tilmap_dist_value_con,
        tilmap_dist_feature_con,
        groups_con,
        group_display_choice,
        plot_colors,
        key_col = "label"
    )

    
    callModule(data_table_module, "til_table", tilmap_tbl, escape = F)
    
    
    
}