immunomodulator_UI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    titleBox("Immunomodulators"),
    textBox(
      width = 12,
      p("Some overview/summary text describing this module and the data presented within.")  
    ),
    
    # Immunomodulator distributions section ----
    sectionBox(
      title = "Immunomodulator Distributions",
      messageBox(
        width = 12,
        p("Brief instructional message about this section, what to do in it, and the available options.")  
      ),
      fluidRow(
        optionsBox(width = 4,
                   
                   # Drop-down selected immuno modulator
                   selectInput(
                     inputId = ns("im_choice"),
                     label = "Select Immunomodulator Gene",
                     choices = panimmune_data$im_direct_relationships[["HGNC Symbol"]]
                   )
        ),
        
        plotBox(width = 8,
                # Show a plot of the generated distribution
                plotlyOutput(ns("boxPlot")),
                plotlyOutput(ns("histPlot"))
        )
      )
    ),
    
    # Immunomodulator annotations section ----
    sectionBox(
      title = "Immunomodulator Annotations",
      messageBox(
        width = 12,
        p("Brief instructional message about this section, what to do in it, and the available options.")  
      ),
      fluidRow(
        tableBox(
          width = 12
        )
      )
    )
  )
}

immunomodulator <- function(input, output, session, ss_choice, subset_df){
  
  ss_group <- reactive(get_variable_internal_name(ss_choice()))
  boxplot_df <- reactive(build_boxplot_df(subset_df(), input$im_choice, ss_group()))
  
  output$boxPlot <- renderPlotly({
    print(input$im_choice)
    plot_colors <- decide_plot_colors(panimmune_data, ss_group())
    plot <- create_boxplot(
      boxplot_df(), 
      x = ss_group(), 
      y = "log_count", 
      fill_factor = ss_group(), 
      x_label = ss_choice(), 
      y_label = "Log10 (Count + 1)",
      fill_colors = plot_colors,
      title = get_im_display_name(input$im_choice))
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