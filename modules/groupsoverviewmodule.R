groupsoverview_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    titleBox("Sample Groups Overview"),
    textBox(
      width = 12,
      p("Some overview/summary text describing this module and the data presented within.")  
    ),
    sectionBox(
      title = "Group Key",
      messageBox(
        width = 12,
        p("Brief instructional message about this section, what to do in it, and the available options.")  
      ),
      fluidRow(
        tableBox(
          width = 12,
          title = textOutput(ns("sample_group_name")),
          div(style = "overflow-x: scroll",
              DT::dataTableOutput(ns("sample_group_table"))
          )
        )
      )
    ),
    sectionBox(
      title = "Group Overlap",
      messageBox(
        width = 12,
        p("Brief instructional message about this section, what to do in it, and the available options.")  
      ),
      fluidRow(
        optionsBox(
          width = 8,
          uiOutput(ns("mosaic_group_select"))
        ),
        uiOutput(ns("study_subset_select"))
      ),
      fluidRow(
        plotBox(
          width = 12,
          column(
            width = 12,
            plotlyOutput(ns("mosaicPlot"), height = "600px")
          )
        )
      )
    )
  )
}

groupsoverview <- function(input, output, session, ss_choice, subset_df, width) {
  ns <- session$ns
  
  ss_internal <- reactive(get_variable_internal_name(ss_choice()))
  sample_groups <- reactive(get_category_group(ss_internal()))
  
  output$sample_group_name <- renderText({
    paste(ss_choice(), "Groups")
  })
  
  output$sample_group_table <- DT::renderDataTable({
    
    sample_group_colors <- decide_plot_colors(
      panimmune_data, ss_internal()
    )
    
    build_sample_group_key_df(
      df = subset_df(),
      sample_group_option = ss_internal()
    ) %>% 
      datatable(
        options = list(
          dom = "tip",
          pageLength = 10,
          columnDefs = list(
            list(width = '50px', targets = c(1))
          )
        ),
        rownames = FALSE
      ) %>%
      formatStyle(
        "Plot Color",
        backgroundColor = styleEqual(
          sample_group_colors, 
          sample_group_colors
        )
      )
  })
  
  output$mosaic_group_select <- renderUI({
    choices <- as.character(
      panimmune_data$sample_group_names
    ) %>% 
      setdiff(ss_choice())
    
    radioButtons(ns("sample_mosaic_group"), 
                 "Select second sample group to view overlap:",
                 choices = choices,
                 selected = choices[1],
                 inline = TRUE)
  })
  
  output$study_subset_select <- renderUI({
    
    req(input$sample_mosaic_group, cancelOutput = TRUE)
    print(input$sample_mosaic_group == "TCGA Subtype")
    
    if (input$sample_mosaic_group == "TCGA Subtype") {
      choices <- panimmune_data$fmx_df %>%
        filter_at(
          vars(get_variable_internal_name(input$sample_mosaic_group)), 
          all_vars(!is.na(.))
        ) %>% 
        distinct(Study) %>%
        extract2("Study")
      
      optionsBox(
        width = 4,
        selectInput(ns("study_subset_selection"), 
                    "Choose study subset:",
                    choices = choices,
                    selected = NULL)
      )
    }
  })
  
  output$mosaicPlot <- renderPlotly({
      print(width())
    
    req(input$sample_mosaic_group, input$study_subset_selection,
        cancelOutput = T)
    
    display_x  <- input$sample_mosaic_group
    display_y  <- ss_choice()
    
    internal_x <- get_variable_internal_name(display_x)
    internal_y <- get_variable_internal_name(display_y)
    
    subset_df() %>% 
      build_mosaic_plot_df(
        x_column = internal_x,
        y_column = internal_y,
        study_value = input$study_subset_selection
      ) %>% 
      create_mosaicplot(
        x = internal_x,
        y = internal_y,
        fill_factor = internal_y,
        xlab = display_x,
        ylab = display_y,
        fill_colors = decide_plot_colors(panimmune_data, internal_y),
        width = (3 * width() )/ 4
      ) 
  })
  
}

