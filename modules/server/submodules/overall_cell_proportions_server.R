overall_cell_proportions_server  <- function(
    input, 
    output, 
    session,
    sample_tbl,
    group_tbl
){
    
    source("functions/overall_cell_proportions_functions.R", local = T)
    
    value_tbl <- shiny::reactive({
        shiny::req(sample_tbl())
        build_value_tbl(sample_tbl())

    })
    
    barplot_tbl <- shiny::reactive({
        req(value_tbl())
        build_barplot_tbl(value_tbl())
    })
    
    output$barplot <- plotly::renderPlotly({
        shiny::req(barplot_tbl())
        
        shiny::validate(shiny::need(
            nrow(barplot_tbl()) > 0, 
            "Samples in current selected groups have no fraction data."))
        
        .GlobalEnv$create_barplot(
            barplot_tbl(),
            color_col = "color",
            label_col = "label",
            xlab = "Fraction type by group",
            ylab = "Fraction mean",
            source_name = "op_barplot"
        )
    })
    
    output$barplot_text <- shiny::renderText({
        
        shiny::req(group_tbl())
        eventdata <- plotly::event_data("plotly_click", source = "op_barplot")
        shiny::validate(shiny::need(eventdata, "Click above plot"))
        
        selected_group <- eventdata$x[[1]]
        
        group_tbl() %>% 
            dplyr::filter(group == selected_group) %>% 
            dplyr::mutate(text = paste0(name, ": ", characteristics)) %>%
            dplyr::pull(text)
    })
    
    output$scatterplot <- plotly::renderPlotly({
        shiny::req(value_tbl())
        eventdata <- plotly::event_data( "plotly_click", source = "op_barplot")
        shiny::validate(shiny::need(eventdata, "Click above plot"))
        
        
        selected_group <- eventdata$x[[1]]
        groups         <- dplyr::pull(value_tbl(), group)
        shiny::validate(shiny::need(selected_group %in% groups, "Click above barchart"))
        
        scatterplot_tbl <-  build_scatterplot_tbl(
            value_tbl(),
            selected_group
        ) 
        
        .GlobalEnv$create_scatterplot(
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