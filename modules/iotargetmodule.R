iotarget_UI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    titleBox("iAtlas Explorer — IO Targets"),
    textBox(
      width = 12,
      
      p("This module allows you to examine how targets of agents being studied in ongoing clinical trials vary within tumors. IO targets are those from the compendium ",
        tags$a(href="https://www.cancerresearch.org/scientists/clinical-accelerator/landscape-of-immuno-oncology-drug-development","IO Drug Development Landscape"),
        " by the", 
        tags$a(href="https://www.cancerresearch.org","Cancer Research Institute"), 
        "Clinical Accelerator."
         ) 
       ), 
    
    # IO Target distributions section ----
    sectionBox(
      title = "IO Target Gene Expression Distributions",
      messageBox(
        width = 12,
        p("Select an IO target Gene to see its expression in tumor samples. Use Select IO Target Category (drop-down menu on the right) to organize the selection by particular categories. The categories will subsequently appear in the left drop-down menu. The Categories are:"),
        tags$ul(
          tags$li(em('Therapy Type'), ", the type of therapy"),
          tags$li(em('Pathway'), ", the molecular pathway to which the protein belongs")
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
                  choices = c("Therapy Type","Pathway")
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
    input, 
    output, 
    session, 
    group_display_choice, 
    group_internal_choice, 
    sample_group_df,
    subset_df, 
    plot_colors
) {
    
    ns <- session$ns
    
    io_target_expr_plot_df <- reactive({
        
        req(subset_df(), 
            input$io_target_gene_choice, 
            group_internal_choice(),
            cancelOutput = T)
        
        build_io_target_expr_plot_df(
            subset_df(),
            filter_value = input$io_target_gene_choice, 
            group_option = group_internal_choice())
    })
    
    output$violinPlot <- renderPlotly({
        
        req(io_target_expr_plot_df(), cancelOutput = T)
        
        validate(
            need(nrow(io_target_expr_plot_df()) > 0, 
                 "Current selected group and selected variable have no overlap")
        )
        
        create_violinplot(
            io_target_expr_plot_df(),
            xlab = group_display_choice(), 
            ylab = "log10(count + 1)",
            source_name = "violin",
            fill_colors = plot_colors())
    })
        
    
    output$violin_group_text <- renderText({
        req(group_internal_choice(), sample_group_df(), cancelOutput = T)
        
        create_group_text_from_plotly(
            "violin", 
            group_internal_choice(),
            sample_group_df())  
    })
    
    output$histPlot <- renderPlotly({
        
        eventdata <- event_data("plotly_click", source = "violin")
        validate(need(!is.null(eventdata), "Click violin plot above"))
        clicked_group <- eventdata$x[[1]]
        
        current_violin_groups <- io_target_expr_plot_df() %>% 
            magrittr::use_series(x) %>% 
            unique
        
        validate(need(clicked_group %in% current_violin_groups, "Click violin plot above"))
        
        histplot_df <- io_target_expr_plot_df() %>% 
            select(GROUP = x, log_count = y) %>% 
            filter(GROUP == clicked_group)
        
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