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
            select(Class = `Variable Class`, FriendlyLabel, FeatureMatrixLabelTSV) %>% 
            filter(FriendlyLabel %in% get_friendly_numeric_columns()) %>% 
            mutate(Class = ifelse(is.na(Class), "Other", Class)) %>% 
            nest(-Class) %>% 
            mutate(data = map(data, deframe)) %>% 
            deframe()
    }
    
    get_numeric_columns <- function(){
        panimmune_data$fmx_df %>% 
            select_if(is.numeric) %>% 
            colnames()
    }
    
    ns <- NS(id)
    
    tagList(
        titleBox("iAtlas Explorer — TIL Maps"),
        textBox(
            width = 12,
            p("This module relates to the manuscript:"),
            p("Saltz et al. Spatial Organization And Molecular Correlation Of Tumor-Infiltrating Lymphocytes Using Deep Learning On Pathology Images. Cell Reports 23, 181-193, April 3 2018; doi: 10.1016/j.celrep.2018.03.086"),
            p("TCGA H&E digital pathology images were analyzed for tumor infiltrating lymphocytes (TILs) using deep learning. The analysis identifies small spatial regions on the slide image - patches - that are rich in TILs. The resulting pattern of TIL patches are then assessed in multiple ways: in terms of overall counts and spatial density, and patterns identified by computational analysis and scoring by pathologists."),
            p("These assessments are also available in other modules, under the Variable Class: TIL Map Characteristic.")
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
        
        # TIL Map annotations section ----
        sectionBox(
            title = "TIL Map Annotations",
            messageBox(
                width = 12,
                p("The table shows annotations of the TIL Map characteristics. The rightmost column gives access to the TILmaps superimposed on H&E images using the caMicroscope tool. Zoom in to initiate the view of TIL spatial clusters.")  
            ),
            fluidRow(
                tableBox(
                    width = 12,
                    div(style = "overflow-x: scroll",
                        DT::dataTableOutput(ns("til_table")) %>% 
                            shinycssloaders::withSpinner()
                    )
                )
            )
        )
    )
}

# tilmap <- function(input, output, session, ss_choice, subset_df){
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
            select(x = group_internal_choice(), y = input$violin_y, label = "Slide") %>%
            drop_na()
        
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
    
    
    output$til_table <- DT::renderDT({
        
        d <- event_data("plotly_click", source = "plot")
        if (!is.null(d)) {
            slide_ids <- d %>% 
                use_series(key)
            # print(slide_ids)
            data_df <- filter(panimmune_data$fmx_df, Slide %in% slide_ids)
        } else {
            data_df <- panimmune_data$fmx_df
        }
        
        
        TIL_map_columns <- panimmune_data$feature_df %>% 
            filter(`Variable Class` == "TIL Map Characteristic") %>% 
            filter(VariableType == "Numeric") %>% 
            use_series(FeatureMatrixLabelTSV)
        # Slide: column width/wrap problem at the moment for this
        TIL_map_columns_display <- as.character(map(TIL_map_columns,get_variable_display_name))
        
        data_df <- data_df %>% 
            select("ParticipantBarcode", "Study", "Slide",TIL_map_columns) %>% 
            .[complete.cases(.),] %>% 
            mutate(Image = paste(
                "<a href=\"",
                "http://quip1.bmi.stonybrook.edu:443/camicroscope/osdCamicroscope.php?tissueId=",
                Slide,
                "\">",
                Slide,
                "</a>",
                sep="")) %>% select(-Slide)
        colnames(data_df) <- c("ParticipantBarcode", "Study", TIL_map_columns_display,"Image")
        data_df %>% 
            datatable(
                rownames = FALSE,
                escape = setdiff(colnames(.),"Image") ## To get hyperlink displayed
            ) %>% formatRound(TIL_map_columns_display, digits = 1)               
        
        #          ) %>% formatRound(c('til_percentage','NP_mean',"NP_sd","WCD_mean","WCD_sd","CE_mean",
        #                              "CE_sd","Ball_Hall","Banfeld_Raftery","C_index","Det_Ratio","Ball_Hall_Adjusted",
        #                              "Banfeld_Raftery_Adjusted","C_index_Adjusted","Det_Ratio_Adjusted"), digits = 1) 
    })
}