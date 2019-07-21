tilmap_UI <- function(id) {
    
    ns <- NS(id)
    
    tagList(
        titleBox("iAtlas Explorer — TIL Maps"),
        textBox(
            width = 12,
            p("This module relates to the manuscript:"),
            tags$a(
                href = "https://www.cell.com/cell-reports/fulltext/S2211-1247(18)30447-9",
                stringr::str_c(
                    "Saltz et al. Spatial Organization And Molecular",
                    "Correlation Of Tumor-Infiltrating Lymphocytes Using Deep",
                    "Learning On Pathology Images; Cell Reports 23, 181-193,",
                    "April 3 2018.",
                    sep = " "
                )
            ),
            p(stringr::str_c(
                "TCGA H&E digital pathology images were analyzed for tumor",
                "infiltrating lymphocytes (TILs) using deep learning. The",
                "analysis identifies small spatial regions on the slide image",
                "- patches - that are rich in TILs. The resulting pattern of",
                "TIL patches are then assessed in multiple ways: in terms of",
                "overall counts and spatial density, and patterns identified",
                "by computational analysis and scoring by pathologists."
            )),
            p(stringr::str_c(
                "These assessments are also available in other modules, and",
                "are found under the Variable Class: TIL Map Characteristic."
            ))
        ),
        
        distributions_plot_module_UI(
            ns("dist"),
            title_text = "TIL Map Characteristics",
            message_html = p(stringr::str_c(
                "Select a TIL map characteristic to see its distribution over", 
                "sample groups. Plots are available as violin plots, and box",
                "plots with full data points superimposed. Main immune",
                "manuscript context:  If you are looking at immune",
                "subtypes, select TIL Regional Fraction to get Figure 3B.",
                sep = " "
            )),
            click_text = 
                "Click point or violin/box to filter samples in table below"
        ),

        
        data_table_module_UI(
            ns("til_table"),
            title = "TIL Map Annotations",
            message_html = p(
                stringr::str_c(
                    "The table shows annotations of the TIL Map",
                    "characteristics.",
                    "The rightmost column gives access to the ",
                    sep = " "
                ),
                tags$a(
                    href = "http://quip1.bmi.stonybrook.edu:443/select.php",
                    "TIL Maps superimposed on H&E images using the caMicroscope tool: "
                ),
                stringr::str_c(
                    "Zoom in to initiate the view of TIL spatial clusters.",
                    "Colors show regions determined by spatial clustering - a",
                    "view without cluster colors is available through the",
                    "question mark / magnifiying glass selector on upper left",
                    "in caMicroscope.",
                    sep = " "
                )
            )
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
                "http://quip1.bmi.stonybrook.edu:443/camicroscope/osdCamicroscope.php?tissueId=",
                Slide,
                "\">",
                Slide,
                "</a>"
            )) %>%
            dplyr::select(-Slide)
    })
    
    callModule(data_table_module, "til_table", table_df, escape = F)
    
    
    
}