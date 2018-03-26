immunomodulator_UI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    titleBox("Immunomodulators"),
    fluidRow(
      optionsBox(width = 4,
        # Drop-down selected sample groups
        selectInput(
          inputId = ns("ss_choice"),
          label = "Select Sample Groups",
          choices = as.character(
            panimmune_data$sample_selection_choices),
          selected = "Immune Subtype"),
        
        # Drop-down selected immuno modulator
        selectInput(
          inputId = ns("im_choice"),
          label = "Select Immunomodulator Gene",
          choices = as.character(panimmune_data$direct_relationship_modulators$HGNC_Symbol))
      ),
      
      box(width = 8,
        # Show a plot of the generated distribution
        plotlyOutput(ns("boxPlot")),
        plotlyOutput(ns("histPlot"))
      )
    )    
  )
}

immunomodulator <- function(input, output, session){
  
  ss_group <- reactive(get_variable_internal_name(input$ss_choice))
  boxplot_df <- reactive(build_boxplot_df(input$im_choice, ss_group()))
  
  output$boxPlot <- renderPlotly({
    plot_colors <- decide_plot_colors(panimmune_data, ss_group())
    plot <- create_boxplot(
      boxplot_df(), 
      x = ss_group(), 
      y = "log_count", 
      fill_factor = ss_group(), 
      x_label = input$ss_choice, 
      y_label = "Log10 (Count + 1)",
      fill_colors = plot_colors,
      title = get_modulator_display_name(input$im_choice))
    print(ggplotly(plot, source = "select"))
  })
  
  output$histPlot <- renderPlotly({
    
    eventdata <- event_data("plotly_click", source = "select")
    validate(need(!is.null(eventdata), "Click boxplot"))
    
    boxplot_selected_group <- get_selected_group_from_plotly_boxplot(
      boxplot_df(),
      ss_group(), 
      eventdata)
    
    histplot_df <- build_histogram_df(
      boxplot_df(), 
      ss_group(),
      boxplot_selected_group)
    
    plot <- create_histogram(
      histplot_df,
      x = "log_count",
      x_label = "Log10 (Count + 1)",
      title = boxplot_selected_group)
    
    print(ggplotly(plot))
  })
}