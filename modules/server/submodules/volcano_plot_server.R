volcano_plot_server <- function(
    input, 
    output, 
    session, 
    volcano_tbl,
    violin_con,
    volcano_title,
    volcano_source_name,
    fold_change_num,
    fold_change_dem,
    response_name,
    pval_threshold = 0.05,
    fold_change_threshold = 2
){
    volcano_plot_tbl <- shiny::reactive({
        
        shiny::req(volcano_tbl(), pval_threshold, fold_change_threshold)
        volcano_tbl() %>% 
            dplyr::mutate(color = dplyr::if_else(
                p_value < pval_threshold & 
                    abs(fold_change) > fold_change_threshold,
                "red",
                "blue"
            )) %>% 
            dplyr::select(
                x = log10_fold_change, 
                y = log10_p_value,
                label,
                color
            )
    })
    
    output$volcano_plot <- plotly::renderPlotly({
        
        shiny::req(
            volcano_plot_tbl(),
            volcano_title,
            volcano_source_name
        )
        
        shiny::validate(shiny::need(
            nrow(volcano_plot_tbl()) > 0,
            "Current parameters did not result in any successful linear regression results."
        ))
        
        create_scatterplot(
            volcano_plot_tbl(),
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
    
    output$violin_plot <- plotly::renderPlotly({
        
        eventdata <- plotly::event_data("plotly_click", source = volcano_source_name)

        # plot not clicked on yet
        shiny::validate(shiny::need(
            !is.null(eventdata),
            "Click a point on the above scatterplot to see a violin plot for the comparison"
        ))
        
        label_value <- eventdata$key[[1]]
        # plot clicked on but event data stale due to parameter change
        shiny::validate(shiny::need(
            label_value %in% dplyr::pull(violin_con(), label),
            "Click a point on the above scatterplot to see a violin plot for the comparison"
        ))
    

        title <- create_volcano_drilldown_plot_title(
            volcano_tbl(), 
            label_value,
            fold_change_num,
            fold_change_dem
        )
        
        violin_plot_tbl <- violin_con() %>% 
            dplyr::filter(label == label_value) %>% 
            dplyr::select(x = status, y = response) %>% 
            dplyr::collect()
        
        
        create_violinplot(
            violin_plot_tbl,
            xlab = label_value,
            ylab = response_name(),
            title = title,
            fill_colors = c("blue"),
            showlegend = FALSE)
    })
    
    

}