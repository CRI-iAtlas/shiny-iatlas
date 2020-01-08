# -----------------------------------------------------------------------------
cellcontent_UI <- function(id) {
    ns <- NS(id)
    
    tagList(
        titleBox("iAtlas Explorer — Tumor Microenvironment"),
        textBox(
            width = 12,
            p("Explore the immune cell proportions in your sample groups.")  
        ),
        # overall_cell_proportions_module_UI(ns("ocp_module")),
        cell_type_fractions_module_UI(ns("ctf_module"))
    )
}

cellcontent <- function(
    input,
    output, 
    session, 
    feature_values_tbl,
    features_tbl,
    sample_tbl,
    group_tbl
) {
    
    callModule(
        overall_cell_proportions_module, 
        "ocp_module",
        feature_values_tbl,
        features_tbl,
        sample_tbl,
        group_tbl
    )
    
    callModule(
        cell_type_fractions_module, 
        "ctf_module",
        feature_values_tbl,
        features_tbl,
        sample_tbl,
        group_tbl
    )
}

# -----------------------------------------------------------------------------
overall_cell_proportions_module_UI <- function(id){
    
    ns <- NS(id)
    
    sectionBox(
        title = "Overall Cell Proportions",
        messageBox(
            width = 12,
            includeMarkdown("data/markdown/cell_proportions1.markdown")
        ),
        fluidRow(
            plotBox(
                width = 12,
                plotlyOutput(ns("barplot")) %>% 
                    shinycssloaders::withSpinner(),
                p(),
                textOutput(ns("barplot_text"))
            )
        ),
        fluidRow(
            plotBox(
                width = 12,
                messageBox(
                    width = 6,
                    includeMarkdown("data/markdown/cell_proportions2.markdown")
                ),
                column(
                    width = 6,
                    br(),
                    fluidRow(
                        plotlyOutput(ns("scatterplot")) %>%
                            shinycssloaders::withSpinner()
                    )
                )
            )
        )
    )
    
}

overall_cell_proportions_module  <- function(
    input, 
    output, 
    session,
    feature_values_tbl,
    features_tbl,
    sample_tbl,
    group_tbl
){
    
    cp_feature_tbl <- reactive({
        req(features_tbl())
        features_tbl() %>% 
            dplyr::filter(
                feature_name %in% c(
                    "Leukocyte Fraction",
                    "Stromal Fraction",
                    "Tumor Fraction"
                ) 
            )
    })
    
    cp_value_tbl <- reactive({
        req(feature_values_tbl(), cp_feature_tbl())
        feature_values_tbl() %>% 
            dplyr::inner_join(cp_feature_tbl(), by = "feature_id") %>% 
            dplyr::inner_join(sample_tbl(), by = "sample_id") %>% 
            dplyr::select(value, class_name, feature_name, order, sample_name = name, group) 
    })
    
    barplot_tbl <- reactive({
        req(cp_value_tbl())
        build_cell_proportion_barplot_tbl(cp_value_tbl())
    })
    
    output$barplot <- renderPlotly({
        
        req(barplot_tbl())
        
        validate(need(
            nrow(barplot_tbl()) > 0, 
            "Samples in current selected groups have no fraction data."))
        
        create_barplot(
            barplot_tbl(),
            color_col = "color",
            label_col = "label",
            xlab = "Fraction type by group",
            ylab = "Fraction mean",
            source_name = "op_barplot"
        )
    })
    
    output$barplot_text <- renderText({
        req(group_tbl())
        eventdata <- event_data("plotly_click", source = "op_barplot")
        validate(need(eventdata, "Click above plot"))
        
        group_tbl() %>% 
            dplyr::filter(group == unique(dplyr::pull(eventdata, "x"))) %>% 
            dplyr::mutate(text = paste0(name, ": ", characteristics)) %>%
            dplyr::pull(text)
    })
    
    output$scatterplot <- renderPlotly({
        req(cp_value_tbl())
        eventdata <- event_data( "plotly_click", source = "op_barplot")
        validate(need(eventdata, "Click above plot"))
        
        
        selected_group <- eventdata$x[[1]]
        groups         <- dplyr::pull(cp_value_tbl(), group)
        validate(need(selected_group %in% groups, "Click above barchart"))
        
        scatterplot_tbl <-  build_cell_proportion_scatterplot_tbl(
            cp_value_tbl(),
            selected_group
        ) 
        
        create_scatterplot(
            scatterplot_tbl,
            xlab = "Stromal Fraction",
            ylab = "Leukocyte Fraction",
            label_col = "label",
            fill_colors = "blue",
            title = selected_group,
            identity_line = TRUE
        ) %>%
            layout(margin = list(t = 39))
    })
}

# -----------------------------------------------------------------------------
cell_type_fractions_module_UI <- function(id){
    
    ns <- NS(id)
    
    sectionBox(
        title = "Cell Type Fractions",
        messageBox(
            width = 12,
            includeMarkdown("data/markdown/cell_fractions.markdown")
        ),
        fluidRow(
            optionsBox(
                width = 12,
                selectInput(
                    inputId = ns("fraction_group_choice"),
                    label = "Select Cell Fraction Type",
                    choices = c(
                        "Immune Cell Proportion - Common Lymphoid and Myeloid Cell Derivative Class",
                        "Immune Cell Proportion - Differentiated Lymphoid and Myeloid Cell Derivative Class",
                        "Immune Cell Proportion - Multipotent Progenitor Cell Derivative Class",
                        "Immune Cell Proportion - Original"
                    ),
                    selected = "Immune Cell Proportion - Differentiated Lymphoid and Myeloid Cell Derivative Class"
                )
            )
        ),
        fluidRow(
            plotBox(
                width = 12,
                plotlyOutput(ns("barplot")) %>% 
                    shinycssloaders::withSpinner(),
                p(),
                textOutput(ns("barplot_text"))
            )
        )
    )
}

cell_type_fractions_module <- function(
    input, 
    output, 
    session,
    feature_values_tbl,
    features_tbl,
    sample_tbl,
    group_tbl
){
    
    cf_value_tbl <- reactive({
        req(
            features_tbl(),
            feature_values_tbl(),
            sample_tbl(),
            input$fraction_group_choice
        )
        build_cell_fractions_barplot_tbl(
            features_tbl(), 
            feature_values_tbl(),
            sample_tbl(),
            input$fraction_group_choice
        )
    })
    
    output$barplot <- renderPlotly({
        
        req(cf_value_tbl())

        validate(need(
            nrow(cf_value_tbl()) > 0,
            "Samples in current selected groups have no selected fraction data.")
        )

        create_barplot(
            cf_value_tbl(),
            color_col = "color",
            error_col = "error",
            label_col = "label",
            xlab = "Fraction type by group",
            ylab = "Fraction mean",
            source_name = "cf_barplot"
        )

    })
    
    output$barplot_text <- renderText({
        req(group_tbl())
        eventdata <- event_data("plotly_click", source = "cf_barplot")
        validate(need(eventdata, "Click above plot"))
        
        group_tbl() %>% 
            dplyr::filter(group == unique(dplyr::pull(eventdata, "x"))) %>% 
            dplyr::mutate(text = paste0(name, ": ", characteristics)) %>%
            dplyr::pull(text)
    })
    
    
}

