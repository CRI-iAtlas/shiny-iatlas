tumor_microenvironment_server <- function(
    input,
    output, 
    session, 
    feature_values_con,
    features_con,
    group_con
) {
    
    callModule(
        overall_cell_proportions_server, 
        "ocp_module",
        feature_values_con,
        features_con,
        group_con
    )
    
    callModule(
        cell_type_fractions_server, 
        "ctf_module",
        feature_values_con,
        features_con,
        group_con
    )
}

# -----------------------------------------------------------------------------


overall_cell_proportions_server  <- function(
    input, 
    output, 
    session,
    feature_values_con,
    features_con,
    group_con
){
    
    cp_feature_con <- reactive({
        req(features_con())
        features_con() %>% 
            dplyr::filter(
                feature_name %in% c(
                    "Leukocyte Fraction",
                    "Stromal Fraction",
                    "Tumor Fraction"
                ) 
            ) %>% 
            dplyr::select(feature_id, feature_name) %>% 
            dplyr::compute() 
    })
    
    cp_value_con <- reactive({
        req(feature_values_con(), cp_feature_con())
        feature_values_con() %>% 
            dplyr::inner_join(cp_feature_con(), by = "feature_id") %>% 
            dplyr::select(sample_name, feature_name, value, group) %>% 
            dplyr::compute()
    })
    
    barplot_tbl <- reactive({
        req(cp_value_con())
        build_cell_proportion_barplot_tbl(cp_value_con())
    })
    
    output$barplot <- plotly::renderPlotly({
        shiny::req(barplot_tbl())
        
        shiny::validate(shiny::need(
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
        req(group_con())
        eventdata <- plotly::event_data("plotly_click", source = "op_barplot")
        shiny::validate(shiny::need(eventdata, "Click above plot"))
        
        selected_group <- eventdata$x[[1]]
        
        group_con() %>% 
            dplyr::filter(group == selected_group) %>% 
            dplyr::mutate(text = paste0(name, ": ", characteristics)) %>%
            dplyr::pull(text)
    })
    
    output$scatterplot <- plotly::renderPlotly({
        req(cp_value_con())
        eventdata <- plotly::event_data( "plotly_click", source = "op_barplot")
        shiny::validate(shiny::need(eventdata, "Click above plot"))
        
        
        selected_group <- eventdata$x[[1]]
        groups         <- dplyr::pull(cp_value_con(), group)
        shiny::validate(shiny::need(selected_group %in% groups, "Click above barchart"))
        
        scatterplot_tbl <-  build_cell_proportion_scatterplot_tbl(
            cp_value_con(),
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
            plotly::layout(margin = list(t = 39))
    })
}

# -----------------------------------------------------------------------------


cell_type_fractions_server <- function(
    input, 
    output, 
    session,
    feature_values_con,
    features_con,
    group_con
){
    
    cf_value_tbl <- reactive({
        req(
            features_con(),
            feature_values_con(),
            input$fraction_group_choice
        )
        build_cell_fractions_barplot_tbl(
            features_con(), 
            feature_values_con(),
            input$fraction_group_choice
        )
    })
    
    output$barplot <- plotly::renderPlotly({
        
        shiny::req(cf_value_tbl())

        shiny::validate(shiny::need(
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
        req(group_con())
        eventdata <- plotly::event_data("plotly_click", source = "cf_barplot")
        shiny::validate(shiny::need(eventdata, "Click above plot"))
        
        selected_group <- eventdata$x[[1]]
        
        group_con() %>% 
            dplyr::filter(group == selected_group) %>% 
            dplyr::mutate(text = paste0(name, ": ", characteristics)) %>%
            dplyr::pull(text)
    })
    
    
}

