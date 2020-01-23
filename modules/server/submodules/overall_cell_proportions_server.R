overall_cell_proportions_server  <- function(
    input, 
    output, 
    session,
    sample_tbl,
    group_tbl
){
    
    source("modules/server/submodules/plotly_server.R", local = T)
    source("functions/overall_cell_proportions_functions.R", local = T)
    
    value_tbl <- shiny::reactive({
        shiny::req(sample_tbl())
        build_value_tbl(sample_tbl())

    })
    
    barplot_tbl <- shiny::reactive({
        req(value_tbl())
        build_barplot_tbl(value_tbl())
    })
    
    barplot_eventdata <- shiny::callModule(
        plotly_server,
        "plotly_barplot",
        plot_tbl = barplot_tbl,
        plot_function = shiny::reactive(.GlobalEnv$create_barplot),
        plot_source_name = "overall_cell_proportions_barplot",
        color_col = "color",
        label_col = "label",
        xlab = "Fraction type by group",
        ylab = "Fraction mean",
    )
    
    barplot_selected_group <- reactive({
        shiny::req(barplot_eventdata())
        selected_group <- barplot_eventdata()$x[[1]]
    })
    
    scatterplot_tbl <- reactive({
        shiny::validate(shiny::need(barplot_eventdata(), "Click above plot"))
        shiny::req(value_tbl(), barplot_selected_group())
        
        groups         <- dplyr::pull(value_tbl(), group)
        shiny::validate(shiny::need(
            barplot_selected_group() %in% groups, 
            "Click above barchart"
        ))
        
        build_scatterplot_tbl(
            value_tbl(),
            barplot_selected_group()
        ) 
    })
    
    shiny::callModule(
        plotly_server,
        "plotly_scatterplot",
        plot_tbl = scatterplot_tbl,
        plot_function = shiny::reactive(.GlobalEnv$create_scatterplot),
        plot_source_name = "overall_cell_proportions_scatterplot",
        group_tbl = group_tbl,
        show_group_text = F,
        xlab = "Stromal Fraction",
        ylab = "Leukocyte Fraction",
        label_col = "label",
        fill_colors = "blue",
        title = barplot_selected_group(),
        identity_line = TRUE
    )
}