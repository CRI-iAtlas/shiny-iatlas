iotarget_UI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    titleBox("iAtlas Explorer — IO Targets"),
    textBox(
      width = 12,
      p("Explore the expression of genes that code for IO targets of the IO Drug Development Landscape")  
    ),
    
    # IO Target distributions section ----
    sectionBox(
      title = "IO Target Gene Expression Distributions",
      messageBox(
        width = 12,
        p("Select IO Target Gene to see its expression in the data set. Use Select Immumodulator Category (drop-down menu on the right) to organize the selection by particular categories. The categories will subsequently appear in the left drop-down menu. The Categories are:"),
        tags$ul(
          tags$li(em('Pathway'), ", Pathway")
#          tags$li(em('Super Category'), ", such as Ligand, Receptor, or Antigen Presentation"),
#          tags$li(em('Immune Checkpoint'), " classified as  Inhibitory or Stimulatory")
        ),
        p(""),
        p("")
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
                  inputId = ns("io_target_category_choice_choice"),
                  label = "Select IO Target Category",
                  choices = c("Pathway")
              )
          )
        )
      ),
      fluidRow(
        plotBox(
          width = 12,
          plotlyOutput(ns("violinPlot")) %>% 
            shinycssloaders::withSpinner(),
          p(),
          textOutput(ns("violin_group_text"))
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
    
    # IO Target annotations section ----
    sectionBox(
      title = "IO Target Annotations",
      messageBox(
        width = 12,
        p("The table shows annotations of the IO Targets. Use the Search box in the upper right to find an immumodulator of interest.")  
      ),
      fluidRow(
        tableBox(
          width = 12,
          div(style = "overflow-x: scroll",
              DT::dataTableOutput(ns("io_target_annotations_table")) %>%
                shinycssloaders::withSpinner()
          )
        )
      )
    )
  )
}

iotarget <- function(
    input, output, session, group_display_choice, group_internal_choice, 
    subset_df, plot_colors) {
    
    ns <- session$ns
    
    io_target_expr_plot_df <- reactive(
        df <- 
            build_io_target_expr_plot_df(
                subset_df(),
                filter_value = input$io_target_gene_choice, 
                group_option = group_internal_choice()))
    
    output$violinPlot <- renderPlotly(
        create_violinplot(
            io_target_expr_plot_df(),
            xlab = group_display_choice(), 
            ylab = "log10(count + 1)",
            source_name = "violin",
            fill_colors = plot_colors()))
    
    output$violin_group_text <- renderText(create_group_text_from_plotly("violin"))
    
    output$histPlot <- renderPlotly({
        
        eventdata <- event_data("plotly_click", source = "violin")
        validate(need(!is.null(eventdata), "Click violin plot above"))
        
        histplot_df <- io_target_expr_plot_df() %>% 
            select(GROUP = x, log_count = y) %>% 
            filter(GROUP == eventdata$x[[1]])
        
        create_histogram(
            histplot_df,
            x_col = "log_count",
            x_lab = "log10(count + 1)",
            title = eventdata$x[[1]])
    })
    
    output$io_target_annotations_table <- DT::renderDT({
        
        panimmune_data$io_target_annotations %>% 
            datatable(
                options = list(pageLength = 10),
                rownames = FALSE
                )
    })
    
    output$gene_choices <- renderUI({
        choices <- get_iotarget_nested_list(
            class_column = input$io_target_category_choice_choice)
        selectInput(
            ns("io_target_gene_choice"),
            label = "Select IO Target Gene",
            choices = choices)
    })
    
}