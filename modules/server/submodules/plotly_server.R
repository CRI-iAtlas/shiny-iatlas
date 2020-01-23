plotly_server <- function(
    input, 
    output, 
    session,
    plot_tbl,
    plot_function,
    plot_source_name,
    group_tbl = NULL,
    show_group_text = T,
    ...
){
    
    
    # This is so that the conditional panel can see output$show_group_text
    output$show_group_text <- shiny::reactive(show_group_text)
    shiny::outputOptions(output, "show_group_text", suspendWhenHidden = FALSE)
    
    
    output$plot <- plotly::renderPlotly({
        shiny::req(plot_tbl(), plot_function(), plot_source_name)

        plot_function()(
            df = plot_tbl(),
            source_name = plot_source_name,
            ...
        )
    })
    
    eventdata <- shiny::reactive({
        shiny::req(plot_source_name)
        plotly::event_data(
            "plotly_click", 
            source = plot_source_name
        )
    })
    
    output$plot_group_text <- shiny::renderText({
        shiny::req(group_tbl())
        
        shiny::validate(shiny::need(eventdata(), "Click above plot"))
        
        selected_group <- eventdata()$x[[1]]
        
        group_tbl() %>% 
            dplyr::filter(group == selected_group) %>% 
            dplyr::mutate(text = paste0(name, ": ", characteristics)) %>%
            dplyr::pull(text)
    })
    
    output$download_tbl <- shiny::downloadHandler(
        filename = function() stringr::str_c("data-", Sys.Date(), ".csv"),
        content = function(con) readr::write_csv(plot_tbl(), con)
    )
    
    return(eventdata)
}