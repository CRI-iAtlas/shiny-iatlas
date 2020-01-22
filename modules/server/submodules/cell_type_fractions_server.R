cell_type_fractions_server <- function(
    input, 
    output, 
    session,
    sample_tbl,
    group_tbl
){
    source("functions/cell_type_fractions_functions.R", local = T)
    
    value_tbl <- shiny::reactive({
        shiny::req(input$fraction_group_choice)
        build_value_tbl(input$fraction_group_choice)
    })
    
    output$barplot <- plotly::renderPlotly({
        
        shiny::req(value_tbl())
        
        shiny::validate(shiny::need(
            nrow(value_tbl()) > 0,
            "Samples in current selected groups have no selected fraction data.")
        )
        
        .GlobalEnv$create_barplot(
            value_tbl(),
            color_col = "color",
            error_col = "error",
            label_col = "label",
            xlab = "Fraction type by group",
            ylab = "Fraction mean",
            source_name = "cf_barplot"
        )
    })
    
    output$barplot_text <- shiny::renderText({
        shiny::req(group_tbl())
        eventdata <- plotly::event_data("plotly_click", source = "cf_barplot")
        shiny::validate(shiny::need(eventdata, "Click above plot"))
        
        selected_group <- eventdata$x[[1]]
        
        group_tbl() %>% 
            dplyr::filter(group == selected_group) %>% 
            dplyr::mutate(text = paste0(name, ": ", characteristics)) %>%
            dplyr::pull(text)
    })
    
    
}