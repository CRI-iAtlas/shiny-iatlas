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
                  plotlyOutput(ns("mosaicPlot"), height = "600px")
              )
          )
      )
  )
}

groupsoverview <- function(input, output, session, ss_choice, subset_df) {
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
        group_color_df <- sample_group_colors %>% 
            tibble() %>% 
            set_names("Color") %>% 
            mutate(`Sample Group` = names(sample_group_colors)) %>% 
            distinct() %>% 
            filter(`Sample Group` %in% subset_df()[[ss_internal()]]) %>% 
            mutate(`Group Size` = "?") %>% 
            select(one_of("Sample Group", "Group Size", "Color")) %>% 
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
                "Color",
                backgroundColor = styleEqual(
                    sample_group_colors, 
                    sample_group_colors
                )
            )
    })
    
    output$mosaic_group_select <- renderUI({
        choices <- as.character(
            panimmune_data$sample_selection_choices
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
        
        if (input$sample_mosaic_group == "TCGA Subtype") {
            choices <- panimmune_data$df %>%
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
        
        req(input$sample_mosaic_group, input$study_subset_selection,
            cancelOutput = T)
        
        display_x  <- input$sample_mosaic_group
        display_y  <- ss_choice()
        
        internal_x <- get_variable_internal_name(display_x)
        internal_y <- get_variable_internal_name(display_y)

        plot_df <- let(
            alias = c(COLX = internal_x,
                      COLY = internal_y),
            subset_df() %>%
                subset_panimmune_df(
                    internal_x, 
                    input$study_subset_selection
                ) %>% 
                select(COLX, COLY) %>%
                .[complete.cases(.),] %>%
                mutate(COLX = as.factor(COLX)) %>%
                mutate(COLY = as.factor(COLY)))

        create_mosaicplot(
            plot_df,
            internal_x,
            internal_y,
            internal_y,
            xlab = display_x,
            ylab = display_y,
            fill_colors = decide_plot_colors(panimmune_data, internal_y)
        ) %>% 
            layout(autosize = FALSE, height = 600)
    })

}

