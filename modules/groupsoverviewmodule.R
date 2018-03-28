groupsoverview_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
      titleBox("Sample Groups Overview"),
      sectionBox(
          title = "Group Key",
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
                  plotOutput(ns("mosaicPlot"))
              )
          )
      ),
      sectionBox(
          title = "Feature Trends",
          fluidRow(
              tabBox(
                  width = 12,
                  tabPanel(
                      "Distributions",
                      fluidRow(
                          optionsBox(
                              width = 6,
                              selectInput(
                                  ns("violin_y"),
                                  "Select violin plot y variable",
                                  choices = get_friendly_numeric_columns(),
                                  selected = "leukocyte_fraction"
                              )
                          )
                      ),
                      fluidRow(
                          plotBox(
                              width = 12,
                              plotlyOutput(ns("violinPlot"))
                          )
                      )
                  ),
                  tabPanel(
                      "Correlations",
                      fluidRow(
                          optionsBox(
                              width = 12,
                              column(
                                  width = 8,
                                  selectInput(
                                      ns("heatmap_y"),
                                      "Select heatmap y variable",
                                      c(
                                          "Core Expression Signature",
                                          "DNA Alteration",
                                          "Adaptive Receptor",
                                          "T Helper Cell Score",
                                          "Immune Cell Proportion - Original",
                                          "Immune Cell Proportion - Aggregate 1",
                                          "Immune Cell Proportion - Aggregate 2",
                                          "Immune Cell Proportion - Aggregate 3"
                                      ),
                                      selected = "Immune Cell Proportion - Aggregate 2"
                                  )
                              ),
                              column(
                                  width = 4,
                                  selectInput(
                                      ns("heatmap_values"),
                                      "Select heatmap values",
                                      c(
                                          "Leukocyte Fraction" = "leukocyte_fraction",
                                          "OS Time" = "OS_time",
                                          "Mutation Rate, Non-Silent" = "mutationrate_nonsilent_per_Mb",
                                          "Indel Neoantigens" = "indel_neoantigen_num",
                                          "SNV Neoantigens" = "numberOfImmunogenicMutation",
                                          "Stemness Score RNA" = "StemnessScoreRNA"
                                      ),
                                      selected = "Leukocyte Fraction"
                                  )
                              )
                          )
                      ),
                      fluidRow(
                          plotBox(
                              width = 6,
                              plotlyOutput(ns("corrPlot"))
                          ),
                          plotBox(
                              width = 6,
                              plotlyOutput(ns("scatterPlot"))
                          )
                      )
                  )
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
            as.data.frame() %>% 
            set_names("Color") %>% 
            rownames_to_column("Sample Group") %>% 
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
                ),
                backgroundSize = '50% 50%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'center'
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
    
    output$mosaicPlot <- renderPlot({
        
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

        plot <- create_mosaicplot(
            plot_df,
            internal_x,
            internal_y,
            internal_y,
            xlab = display_x,
            ylab = display_y,
            fill_colors = decide_plot_colors(panimmune_data, internal_y)
        )
        plot
    })

    output$violinPlot <- renderPlotly({
        
        display_x  <- ss_choice()
        display_y  <- input$violin_y
        internal_x <- get_variable_internal_name(display_x)
        internal_y <- get_variable_internal_name(display_y)
        
        plot_df <- subset_df() %>%
            select_(.dots = c(internal_x, internal_y)) %>%
            .[complete.cases(.),]

        plot_df %>% 
            create_violinplot(
                internal_x,
                internal_y,
                internal_x,
                xlab = display_x,
                ylab = display_y,
                fill_colors = decide_plot_colors(panimmune_data, internal_x)
            )
    })
    
    hm_variables  <- reactive(
        as.character(get_variable_group(input$heatmap_y))
    )
    intermediate_corr_df <- reactive(create_intermediate_corr_df(
        subset_df(),
        input$heatmap_values,
        ss_internal(),
        sample_groups(),
        hm_variables()
    ))

    output$corrPlot <- renderPlotly({
        heatmap_corr_mat <- create_heatmap_corr_mat(
            intermediate_corr_df(),
            input$heatmap_values,
            ss_internal(),
            sample_groups(),
            hm_variables()
        )
        
       create_plotly_heatmap(heatmap_corr_mat)
    })

    output$scatterPlot <- renderPlotly({
        eventdata <- event_data("plotly_click", source = "heatplot")
        validate(need(!is.null(eventdata), "Click heatmap"))

        internal_variable_name <- eventdata$y[[1]] %>%
            get_variable_internal_name() %>%
            .[. %in% colnames(intermediate_corr_df())]
        
        plot_df <- create_scatterplot_df(
            intermediate_corr_df(),
            ss_internal(),
            eventdata$x[[1]],
            internal_variable_name,
            input$heatmap_values
        )
        
        plot_df %>%
            create_scatterplot(
                input$heatmap_values,
                internal_variable_name,
                get_variable_display_name(input$heatmap_values),
                eventdata$y[[1]],
                eventdata$x[[1]])
    })
}

