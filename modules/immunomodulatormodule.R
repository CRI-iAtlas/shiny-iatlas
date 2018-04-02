immunomodulator_UI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    titleBox("Immunomodulators"),
    textBox(
      width = 12,
      p("Explore the expression of genes that code for immunomodulating proteins, including checkpoint proteins.")  
    ),
    
    # Immunomodulator distributions section ----
    sectionBox(
      title = "Immunomodulator Distributions",
      messageBox(
        width = 12,
        p("Select an immumodulator gene to see its expression over sample groups."),
        p("Manuscript context:  If you are looking at immune subtypes, select EDNRB or CXCL10 to get figure 6B.")
      ),
      fluidRow(
        optionsBox(
          width = 4,
          selectInput(
            inputId = ns("im_choice"),
            label = "Select Immunomodulator Gene",
            choices = panimmune_data$im_direct_relationships[["HGNC Symbol"]]
          )
        )
      ),
      fluidRow(
        plotBox(
          width = 12,
          plotlyOutput(ns("violinPlot")) %>% 
            shinycssloaders::withSpinner()
        )
      ),
      fluidRow(
        plotBox(
          width = 12,
          plotlyOutput(ns("histPlot")) %>% 
            shinycssloaders::withSpinner()
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
          width = 12,
          div(style = "overflow-x: scroll",
              DT::dataTableOutput(ns("im_annotations_table")) %>% 
                shinycssloaders::withSpinner()
          )
        )
      )
    )
  )
}

immunomodulator <- function(input, output, session, ss_choice, subset_df){
  
  ss_group <- reactive(get_variable_internal_name(ss_choice()))
  im_expr_plot_df <- reactive(build_im_expr_plot_df(
    subset_df(), input$im_choice, ss_group()
  ))
  
  output$violinPlot <- renderPlotly({
    plot_colors <- decide_plot_colors(panimmune_data, ss_group())
    im_expr_plot_df() %>% 
      create_violinplot(
        x = ss_group(), 
        y = "log_count", 
        fill_factor = ss_group(), 
        xlab = ss_choice(), 
        ylab = "log10(count + 1)",
        source_name = "select",
        fill_colors = plot_colors,
        title = get_im_display_name(input$im_choice))
  })
  
  output$histPlot <- renderPlotly({
    
    eventdata <- event_data("plotly_click", source = "select")
    validate(need(!is.null(eventdata), "Click violin plot"))
    violinplot_selected_group <- im_expr_plot_df() %>% 
      get_selected_group_from_violinplot(
        ss_group(), 
        eventdata
      )

    histplot_df <- build_histogram_df(
      im_expr_plot_df(), 
      ss_group(),
      violinplot_selected_group
    )
    
    histplot_df %>% 
      create_histogram(
        x_column  = "log_count",
        x_lab = "log10(count + 1)",
        title = violinplot_selected_group
      )
  })
  
  output$im_annotations_table <- DT::renderDT({

    panimmune_data$im_direct_relationships %>% 
      select(-X10, -Notes) %>% 
      datatable(
        options = list(
          dom = "tip",
          pageLength = 10
        ),
        rownames = FALSE
      )
  })
}