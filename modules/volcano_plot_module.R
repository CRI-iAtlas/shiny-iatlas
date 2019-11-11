volcano_plot_module_ui <- function(id){
    ns <- NS(id)
    tagList(
        fluidRow(
            plotBox(
                width = 12,
                plotlyOutput(ns("volcano_plot")) %>% 
                    shinycssloaders::withSpinner()
            )
        ),
        fluidRow(
            plotBox(
                width = 12,
                plotlyOutput(ns("violin_plot")) %>%
                    shinycssloaders::withSpinner()
            )
        )
    )
}

volcano_plot_module <- function(
    input, 
    output, 
    session, 
    volcano_con,
    violin_value_con,
    violin_group_con,
    volcano_title,
    volcano_source_name,
    fold_change_num,
    fold_change_dem,
    pval_threshold = 0.05,
    fold_change_threshold = 2
){
    volcano_plot_con <- reactive({
        req(volcano_con(), pval_threshold, fold_change_threshold)
        volcano_con() %>% 
            dplyr::mutate(color = dplyr::if_else(
                pvalue < pval_threshold & abs(fold_change) > fold_change_threshold,
                "red",
                "blue"
            )) %>% 
            dplyr::select(
                x = log10_fold_change, 
                y = log10_pvalue, 
                label,
                color
            )
    })
    
    output$volcano_plot <- renderPlotly({
        create_scatterplot(
            dplyr::as_tibble(volcano_plot_con()),
            xlab = "Log10(Fold Change)",
            ylab = "- Log10(P-value)",
            title = volcano_title,
            source = volcano_source_name,
            key_col = "label",
            label_col = "label",
            color_col = "color",
            fill_colors = c("blue" = "blue", "red" = "red")
        )
    })
    
    output$violin_plot <- renderPlotly({
        
        eventdata <- event_data("plotly_click", source = volcano_source_name)

        # plot not clicked on yet
        validate(need(
            !is.null(eventdata),
            "Click a point on the above scatterplot to see a violin plot for the comparison"
        ))
        
        label_value <- eventdata$key[[1]]
        
        # plot clicked on but event data stale due to parameter change
        validate(need(
            label_value %in% dplyr::pull(violin_group_con(), label),
            "Click a point on the above scatterplot to see a violin plot for the comparison"
        ))

        title <- create_volcano_drilldown_plot_title(
            volcano_con(), 
            label_value,
            fold_change_num,
            fold_change_dem
        )
        
        violin_plot_tbl <- violin_group_con() %>% 
            dplyr::filter(label == label_value) %>% 
            dplyr::inner_join(violin_value_con(), by = "sample") %>% 
            dplyr::select(sample, x = status, y = value) %>% 
            dplyr::as_tibble()
        
        create_violinplot(
            violin_plot_tbl,
            xlab = label_value,
            ylab = "feature",
            title = title,
            fill_colors = c("blue"),
            showlegend = FALSE)
    })
    
    

}