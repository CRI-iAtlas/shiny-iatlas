immunomodulator_UI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    titleBox("iAtlas Explorer — Immune Subtype Predictor"),
    textBox(
      width = 12,
      p("Upload gene expression* and predict immune subtypes (* RSEM RPKM).")  
    ),
    
    # Immunomodulator distributions section ----
    sectionBox(
      title = "Model based clustering",
      messageBox(
        width = 12,
        p("Upload gene expression (csv or tsv). RSEM RPKM expression values were used to train the model, and for best results, your expression data should also be RSEM RPKMs. There are several settings:"),
        tags$ul(
          tags$li(em('Log 10'), ", if the data is not already log transformed, select this."), 
          tags$li(em('Ensemble size'), ", try different ensemble sizes to check for robust results."),
          tags$li(em('File separator'), ", select commas or tabs.")
        ),
        p(""),
        p("Manuscript context:  See figure 1A."),
        p(".....")
      ),
      fluidRow(
        optionsBox(
          width = 12,
          column(
              width = 3,
              radioButtons("sep", "File Separator",
                           choices = c(Comma = ",", Tab = "\t"), selected = ","),
              checkboxInput("logged", "Apply Log10", TRUE)
          ),
          column(
            width = 6,
            fileInput("file1", "Choose CSV file with \n1st column gene symbols",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv",
                                 ".csv.gz",
                                 "text/tsv",
                                 "text/comma-separated-values,text/plain",
                                 ".tsv",
                                 ".tsv.gz"),
                      placeholder = 'data/ivy20.csv')
          ),
          column(
              width = 3,
              numericInput("corenum", "Cores", 4, width = '100'),
              numericInput("ensemblenum", "Ensemble Size", 256, max = 256, min = 32, width = '100')
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
        p("The table shows annotations of the immumodulators, and source. Use the Search box in the upper right to find an immumodulator of interest.")  
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

subtypepredictor <- function(
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