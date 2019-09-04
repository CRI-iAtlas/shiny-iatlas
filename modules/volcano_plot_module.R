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
    volcano_df,
    violin_df,
    volcano_title,
    volcano_source_name,
    fold_change_num,
    fold_change_dem,
    pval_threshold = 0.05,
    log10_fold_change_threshold = log10(2)
){
    volcano_plot_df <- reactive({
        volcano_df() %>% 
            dplyr::mutate(color = dplyr::if_else(
                pvalue > pval_threshold |
                    abs(log10_fold_change) < log10_fold_change_threshold,
                "blue",
                "red"
            )) %>% 
            dplyr::select(
                x = log10_fold_change, 
                y = log10_pvalue, 
                label = LABEL,
                color
            )
    })
    
    output$volcano_plot <- renderPlotly({
        create_scatterplot(
            volcano_plot_df(),
            xlab = "Log10(Fold Change)",
            ylab = "- Log10(P-value)",
            title = volcano_title,
            source = volcano_source_name,
            key_col = "label",
            label_col = "label",
            color_col = "color",
            fill_colors = c("blue" = "blue", "red" = "red"),
            horizontal_line = T,
            horizontal_line_y = (- log10(0.05))
        )
    })
    
    output$violin_plot <- renderPlotly({
        
        eventdata <- event_data("plotly_click", source = volcano_source_name)
        
        # plot not clicked on yet
        validate(need(
            !is.null(eventdata),
            "Click a point on the above scatterplot to see a violin plot for the comparison"
        ))
        
       
        
        group       <- eventdata$key[[1]]
        df          <- dplyr::filter(volcano_df(), LABEL == group)
        metric      <- get_variable_display_name(df$METRIC)
        pvalue      <- round(df$pvalue, 4)
        fc          <- round(df$fold_change, 4)
        fc_string   <- stringr::str_c(
            fold_change_num,
            "/",
            fold_change_dem
        )
        
        
        if(fc < 1) {
            fc        <- round(1/fc, 4)
            fc_string <- stringr::str_c(
                fold_change_dem,
                "/",
                fold_change_num
            )
        }
        
        title <- stringr::str_c(
            "Cohort:",
            group,
            "; P = ",
            pvalue,
            ";",
            fc_string,
            ":", 
            fc,
            sep = " "
        )
        
        # plot clicked on but event data stale due to parameter change
        validate(need(
            group %in% violin_df()$LABEL,
            "Click a point on the above scatterplot to see a violin plot for the comparison"
        ))
        
        violin_plot_df <- violin_df() %>% 
            dplyr::filter(LABEL == group) %>% 
            dplyr::select(x = GROUP, y = METRIC)

        create_violinplot(
            violin_plot_df,
            xlab = group,
            ylab = metric,
            title = title,
            fill_colors = c("blue"),
            showlegend = FALSE)
    })
    
    

}