immune_feature_distributions_server <- function(
    input,
    output, 
    session,
    sample_tbl,
    group_tbl,
    group_name,
    feature_named_list,
    plot_colors
){
    
    ns <- session$ns
    
    source("functions/immune_feature_distributions_functions.R", local = T)
    
    leukocyte_fraction_id <- shiny::reactive(get_leukocyte_fraction_id())
    
    output$selection_ui <- shiny::renderUI({
        req(leukocyte_fraction_id(), feature_named_list())
        
        shiny::selectInput(
            ns("feature_choice_id"),
            label = "Select or Search for Variable",
            selected = leukocyte_fraction_id(),
            choices = feature_named_list()
        )
    })
    
    feature_name <- shiny::reactive({
        shiny::req(
            sample_tbl(),
            input$feature_choice_id,
            input$scale_method
        )
        get_feature_name(input$feature_choice_id)
    })
    
    feature_plot_label <- shiny::reactive({
        shiny::req(
            feature_name(),
            input$scale_method
        )
        .GlobalEnv$transform_feature_string(
            feature_name(), 
            input$scale_method
        )
    })
    
    # distplot ----------------------------------------------------------------
    distplot_tbl <- shiny::reactive({
        shiny::req(
            sample_tbl(), 
            input$feature_choice_id,
            input$scale_method
        )
        build_distplot_tbl(
            sample_tbl(), 
            input$feature_choice_id,
            input$scale_method
        )
    })
    
    distplot_function <- shiny::reactive({
        switch(
            input$plot_type,
            "Violin" = create_violinplot,
            "Box" = create_boxplot
        )
    })
    
    output$distplot <- plotly::renderPlotly({
        distplot_function()(
            distplot_tbl(),
            source_name = "immune_feature_dist_plot",
            xlab = group_name(),
            ylab = feature_plot_label(),
            title = feature_name(),
            fill_colors = plot_colors()
        )
    })
    
    distplot_eventdata <- shiny::reactive({
        plotly::event_data(
            "plotly_click", 
            source = "immune_feature_dist_plot"
        )
    })
    
    distplot_selected_group <- reactive({
        shiny::req(distplot_eventdata())
        selected_group <- distplot_eventdata()$x[[1]]
    })
    
    output$distplot_group_text <- shiny::renderText({
        shiny::validate(shiny::need(distplot_eventdata(), "Click above plot"))
        shiny::req(
            group_tbl(),
            distplot_selected_group()
        )  
        
        group_tbl() %>% 
            dplyr::filter(group == distplot_selected_group()) %>% 
            dplyr::mutate(text = paste0(name, ": ", characteristics)) %>%
            dplyr::pull(text)
    })
    
    output$download_tbl <- shiny::downloadHandler(
        filename = function() stringr::str_c("data-", Sys.Date(), ".csv"),
        content = function(con) readr::write_csv(plot_tbl(), con)
    )
    
    histplot_tbl <- reactive({
        shiny::validate(shiny::need(distplot_eventdata(), "Click above plot"))
        shiny::req(distplot_tbl(), distplot_selected_group())
        
        groups <- dplyr::pull(distplot_tbl(), "x")
        shiny::validate(shiny::need(
            distplot_selected_group() %in% groups, 
            "Click above barchart"
        ))
        
        print(feature_name())
        distplot_tbl() %>% 
            dplyr::filter(x == distplot_selected_group()) %>% 
            dplyr::select(x = y)
    })
    
    output$histplot <- plotly::renderPlotly({
        .GlobalEnv$create_histogram(
            histplot_tbl(),
            source_name = "immune_feature_histogram",
            x_lab = feature_plot_label(),
            y_lab = "Count",
            title = distplot_selected_group(),
        )
    })
}