cell_type_fractions_server <- function(
    input, 
    output, 
    session,
    sample_tbl,
    group_tbl
){
    
    source("modules/server/submodules/plotly_server.R", local = T)
    source("functions/cell_type_fractions_functions.R", local = T)
    
    plot_tbl <- shiny::reactive({
        shiny::req(input$fraction_group_choice)
        build_plot_tbl(input$fraction_group_choice)
    })
    
    shiny::callModule(
        plotly_server,
        "plotly_barplot",
        plot_tbl = plot_tbl,
        plot_function = shiny::reactive(.GlobalEnv$create_barplot),
        plot_source_name = "cell_type_fractions_barplot",
        group_tbl = group_tbl,
        color_col = "color",
        error_col = "error",
        label_col = "label",
        xlab = "Fraction type by group",
        ylab = "Fraction mean",
    )
}