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
          width = 12,
          column(
              width = 6,
              uiOutput(ns("gene_choices"))
          ),
          column(
              width = 6,
              selectInput(
                  inputId = ns("im_category_choice_choice"),
                  label = "Select Immunomodulator Category",
                  choices = c("Gene Family", "Super Category", "Immune Checkpoint")
              )
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
    
    ns <- session$ns
    
    im_expr_plot_df <- reactive(
        df <- 
            build_im_expr_plot_df(
                subset_df(),
                filter_value = input$im_gene_choice, 
                group_option = group_internal_choice()))
    
    output$violinPlot <- renderPlotly(
        create_violinplot(
            im_expr_plot_df(),
            xlab = group_display_choice(), 
            ylab = "log10(count + 1)",
            source_name = "select",
            fill_colors = plot_colors()))
    
    output$histPlot <- renderPlotly({
        
        eventdata <- event_data("plotly_click", source = "select")
        validate(need(!is.null(eventdata), "Click violin plot above"))
        
        histplot_df <- im_expr_plot_df() %>% 
            select(GROUP = x, log_count = y) %>% 
            filter(GROUP == eventdata$x[[1]])
        
        create_histogram(
            histplot_df,
            x_column  = "log_count",
            x_lab = "log10(count + 1)",
            title = eventdata$x[[1]])
    })
    
    output$im_annotations_table <- DT::renderDT({
        
        panimmune_data$im_direct_relationships %>% 
            select(-X10, -Notes) %>% 
            datatable(
                options = list(pageLength = 10),
                rownames = FALSE
                )
    })
    
    output$gene_choices <- renderUI({
        choices <- get_immunomodulator_nested_list(
            class_column = input$im_category_choice_choice)
        selectInput(
            ns("im_gene_choice"),
            label = "Select Immunomodulator Gene",
            choices = choices)
    })
    
}