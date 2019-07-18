tilmap_UI <- function(id) {
    
    get_friendly_numeric_columns <- function(){
        get_numeric_columns() %>% 
            convert_values_between_columns(
                df = panimmune_data$feature_df,
                from_column = "FeatureMatrixLabelTSV",
                to_column = "FriendlyLabel"
            )
    }
    
    get_friendly_numeric_columns_by_group <- function() {
        panimmune_data$feature_df %>% 
            dplyr::select(Class = `Variable Class`, FriendlyLabel, FeatureMatrixLabelTSV) %>% 
            dplyr::filter(FriendlyLabel %in% get_friendly_numeric_columns()) %>% 
            dplyr::mutate(Class = ifelse(is.na(Class), "Other", Class)) %>% 
            tidyr::nest(-Class) %>% 
            dplyr::mutate(data = purrr::map(data, tibble::deframe)) %>% 
            tibble::deframe()
    }
    
    get_numeric_columns <- function(){
        panimmune_data$fmx_df %>% 
            dplyr::select_if(is.numeric) %>% 
            colnames()
    }
    
    ns <- NS(id)
    
    tagList(
        titleBox("iAtlas Explorer — TIL Maps"),
        textBox(
            width = 12,
            p("This module relates to the manuscript:"),
            tags$a(href="https://www.cell.com/cell-reports/fulltext/S2211-1247(18)30447-9","Saltz et al. Spatial Organization And Molecular Correlation Of Tumor-Infiltrating Lymphocytes Using Deep Learning On Pathology Images; Cell Reports 23, 181-193, April 3 2018."),
            p("TCGA H&E digital pathology images were analyzed for tumor infiltrating lymphocytes (TILs) using deep learning. The analysis identifies small spatial regions on the slide image - patches - that are rich in TILs. The resulting pattern of TIL patches are then assessed in multiple ways: in terms of overall counts and spatial density, and patterns identified by computational analysis and scoring by pathologists."),
            p("These assessments are also available in other modules, and are found under the Variable Class: TIL Map Characteristic.")
        ),
        
        # TIL distributions section ----
        sectionBox(
            title = "TIL Map Characteristics",
            messageBox(
                width = 12,
                p("Select a TIL map characteristic to see its distribution over sample groups. Plots are available as violin plots, and box plots with full data points superimposed."),
                p("Main immune manuscript context:  If you are looking at immune subtypes, select TIL Regional Fraction to get Figure 3B.")
            ),
            fluidRow(
                optionsBox(
                    width = 12,
                    column(
                        width = 4,
                        selectInput( ## would be good to initiate on til_percentage/"TIL Regional Fraction (Percent)"
                            ns("violin_y"),
                            "Select TIL Map Characteristic",
                            choices = get_friendly_numeric_columns_by_group()["TIL Map Characteristic"]
                        )
                    ),
                    column(
                        width = 4,
                        selectInput(
                            ns("plot_type"),
                            "Select Plot Type",
                            choices = c("Violin", "Box")
                        )
                    )
                )
            ),
            fluidRow(
                plotBox(
                    width = 12,
                    plotlyOutput(ns("plot")) %>% 
                        shinycssloaders::withSpinner(),
                    p(),
                    textOutput(ns("plot_group_text")),
                    h4("Click point or violin/box to filter samples in table below")
                )
            )
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
    
    ns <- session$ns
    
    
    output$plot <- renderPlotly({
        
        req(!is.null(subset_df()), cancelOutput = T)
        
        display_y  <- get_variable_display_name(input$violin_y)
        
        plot_df <- subset_df() %>%
            dplyr::select(x = group_internal_choice(), y = input$violin_y, label = "Slide") %>%
            tidyr::drop_na()
        
        validate(
            need(nrow(plot_df) > 0, 
                 "Group choices have no overlap with current variable choice"))
        
        if(input$plot_type == "Violin"){
            plot_df %>%
                create_violinplot(
                    xlab = group_display_choice(),
                    ylab = display_y,
                    source_name = "plot",
                    fill_colors = plot_colors(),
                    key_col = "label"
                )
        } else {
            plot_df %>% 
                create_boxplot(
                    xlab = group_display_choice(),
                    ylab = display_y,
                    source_name = "plot",
                    fill_colors = plot_colors(),
                    key_col = "label"
                )
        }
        
    })
    
    output$plot_group_text <- renderText({
        req(group_internal_choice(), sample_group_df(), cancelOutput = T)
        
        create_group_text_from_plotly(
            "plot",
            group_internal_choice(),
            sample_group_df(),
            prompt_text = "",
            key_column = "x")
    })
    
    
    table_df <- reactive({
        
        d <- event_data("plotly_click", source = "plot")
        if (!is.null(d)) {
            slide_ids <- d %>% 
                magrittr::use_series(key)
            # print(slide_ids)
            data_df <- filter(panimmune_data$fmx_df, Slide %in% slide_ids)
        } else {
            data_df <- panimmune_data$fmx_df
        }
        
        
        TIL_map_columns <- panimmune_data$feature_df %>% 
            dplyr::filter(`Variable Class` == "TIL Map Characteristic") %>% 
            dplyr::filter(VariableType == "Numeric") %>% 
            magrittr::use_series(FeatureMatrixLabelTSV)
        # Slide: column width/wrap problem at the moment for this
        TIL_map_columns_display <- as.character(purrr::map(TIL_map_columns,get_variable_display_name))
        
        data_df %>% 
            dplyr::select(
                "ParticipantBarcode", 
                "Study", 
                "Slide",
                TIL_map_columns) %>% 
            tidyr::drop_na() %>% 
            dplyr::mutate(Image = paste(
                "<a href=\"",
                "http://quip1.bmi.stonybrook.edu:443/camicroscope/osdCamicroscope.php?tissueId=",
                Slide,
                "\">",
                Slide,
                "</a>",
                sep="")
            ) %>%
            select(-Slide) %>% 
            magrittr::set_colnames(c(
                "ParticipantBarcode", 
                "Study", 
                TIL_map_columns_display,
                "Image"
            )) %>% 
            dplyr::mutate_if(is.double, round, digits = 1)
    })
    
    callModule(data_table_module, "til_table", table_df(), escape = F)
    
    
    
}