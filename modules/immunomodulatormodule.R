immunomodulator_UI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    titleBox("iAtlas Explorer — Immunomodulators"),
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
        p("Manuscript context:  If you are looking at immune subtypes, select EDNRB or CXCL10 to get figure 6B. You can view a histogram for an indvidual distributions by clicking on its violin.")
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
        p("The table shows annotations of the immumodulators, and source.")  
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

immunomodulator <- function(
    input, output, session, group_display_choice, group_internal_choice, 
    subset_df, plot_colors) {
    
    im_expr_plot_df <- reactive(
        subset_df() %>% 
            build_im_expr_plot_df(
                filter_value = input$im_choice, 
                group_option = group_internal_choice()
            )
    )
    
    output$violinPlot <- renderPlotly(
        create_violinplot(
            im_expr_plot_df(),
            x = group_internal_choice(), 
            y = "log_count", 
            fill_factor = group_internal_choice(), 
            xlab = group_display_choice(), 
            ylab = "log10(count + 1)",
            source_name = "select",
            fill_colors = plot_colors(),
            title = get_im_display_name(input$im_choice)))
    
    output$histPlot <- renderPlotly({
        
        eventdata <- event_data("plotly_click", source = "select")
        validate(need(!is.null(eventdata), "Click violin plot above"))
        violinplot_selected_group <- im_expr_plot_df() %>% 
            get_selected_group_from_violinplot(
                group_internal_choice(), 
                eventdata
            )
        
        histplot_df <- build_histogram_df(
            im_expr_plot_df(), 
            group_internal_choice(),
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