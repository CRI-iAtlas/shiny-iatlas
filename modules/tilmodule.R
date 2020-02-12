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
    group_internal_choice, 
    sample_group_df,
    subset_df,
    plot_colors, 
    group_options){
    
    
    data_df <- reactive({
        dplyr::select(
            subset_df(),
            x = group_internal_choice(), 
            label = "Slide", 
            dplyr::everything()) 
    })
    
    relationship_df <- reactive({
        panimmune_data$feature_df %>% 
            dplyr::filter(`Variable Class` == "TIL Map Characteristic") %>% 
            dplyr::select(
                INTERNAL = FeatureMatrixLabelTSV, 
                DISPLAY = FriendlyLabel,
                `Variable Class`)
    })
    
    callModule(
        distributions_plot_module,
        "dist",
        "tilmap_dist_plot",
        data_df,
        relationship_df,
        sample_group_df,
        plot_colors,
        group_display_choice,
        key_col = "label"
    )
    
    table_df <- reactive({
        
        df <- panimmune_data$fmx_df
        data <- event_data("plotly_click", source = "tilmap_dist_plot")
        if (!is.null(data)) df <- dplyr::filter(df, Slide %in% data$key)
        
        names_df <- panimmune_data$feature_df %>%
            dplyr::filter(`Variable Class` == "TIL Map Characteristic") %>%
            dplyr::filter(VariableType == "Numeric") %>%
            dplyr::select(FeatureMatrixLabelTSV, FriendlyLabel)
        
        df %>% 
            dplyr::select(
                "ParticipantBarcode", 
                "Study", 
                "Slide",
                names_df$FeatureMatrixLabelTSV) %>% 
            tidyr::drop_na() %>% 
            tidyr::gather(FeatureMatrixLabelTSV, value, -c(ParticipantBarcode, Study, Slide)) %>% 
            dplyr::full_join(names_df) %>% 
            dplyr::mutate(value = round(value, digits = 1)) %>% 
            dplyr::select(-FeatureMatrixLabelTSV) %>% 
            tidyr::spread(FriendlyLabel, value) %>% 
            dplyr::mutate(Image = stringr::str_c(
                "<a href=\"",
                "https://quip1.bmi.stonybrook.edu:443/camicroscope/osdCamicroscope.php?tissueId=",
                Slide,
                "\">",
                Slide,
                "</a>"
            )) %>%
            dplyr::select(-Slide)
    })
    
    callModule(data_table_module, "til_table", table_df, escape = F)
    
    
    
}