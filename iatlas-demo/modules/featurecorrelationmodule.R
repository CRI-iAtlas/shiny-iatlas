featurecorrelation_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    titleBox("Immune Feature Correlation Heatmap"),
    fluidRow(
      optionsBox(width = 4,
        selectInput(
          ns("heatmap_values"),
          "Select heatmap values",
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
        ),
        
        selectInput(
            ns("heatmap_x"),
            "Select heatmap x variable",
            choices = purrr::map(config_yaml$immune_groups,
                                 get_variable_display_name),
            selected = "Immune Subtypes"
        ),
        
        selectInput(
          ns("heatmap_y"),
          "Select heatmap y variable",
          c(
            "Leukocyte Fraction" = "leukocyte_fraction",
            "OS Time" = "OS_time",
            "Mutation Rate, Non-Silent" = "mutationrate_nonsilent_per_Mb",
            "Indel Neoantigens" = "indel_neoantigen_num",
            "SNV Neoantigens" = "numberOfImmunogenicMutation",
            "Stemness Score RNA" = "StemnessScoreRNA"
          ),
          selected = "Leukocyte Fraction"
        ),
        
        selectInput(
            ns("violin_x"),
            "Select violin x variable",
            choices = purrr::map(config_yaml$immune_groups,
                                 get_variable_display_name),
            selected = "Immune Subtypes"
        ),
        
        selectInput(
            ns("violin_y"),
            "Select violin plot y variable",
            choices = get_friendly_numeric_columns(),
            selected = "leukocyte_fraction" 
        ),
        
        selectInput(
            ns("mosaic_x"),
            "Select mosaic x variable",
            choices = purrr::map(config_yaml$immune_groups,
                                 get_variable_display_name),
            selected = "TCGA Study"
        ),
        
        selectInput(
            ns("mosaic_y"),
            "Select mosaic y variable",
            choices = purrr::map(config_yaml$immune_groups,
                                 get_variable_display_name),
            selected = "Immune Subtypes"
        )
        

      ),
      
      plotBox(width = 8,
        plotlyOutput(ns("corrPlot")),
        plotlyOutput(ns("scatterPlot")),
        plotOutput(ns("violinPlot")),
        plotOutput(ns("mosaicPlot")),
        HTML("<br><br><br>")
      )
    )
  )
}

featurecorrelation <- function(input, output, session) {
    
    hm_display_x  <- reactive(input$heatmap_x)
    hm_internal_x <- reactive(get_variable_internal_name(hm_display_x()))
    hm_categories <- reactive(get_category_group(hm_internal_x()))
    hm_variables  <- reactive(as.character(get_variable_group(input$heatmap_values)))
    
    df_by_selections <- reactive(filter_data_by_selections(
        input$heatmap_y,
        hm_internal_x(),
        hm_categories(),
        hm_variables()
    ))
    
    output$corrPlot <- renderPlotly({
        corr_matrix <- build_correlation_mat(
            df_by_selections(),
            input$heatmap_y,
            hm_internal_x(),
            hm_categories(),
            hm_variables()
        )
        
        heatmap_plot <- create_plotly_heatmap(corr_matrix)
    })
    
    output$scatterPlot <- renderPlotly({
        eventdata <- event_data("plotly_click", source = "heatplot")
        validate(need(!is.null(eventdata), "Click heatmap"))
        
        internal_variable_name <-
            eventdata$y[[1]] %>%
            get_variable_internal_name() %>%
            .[. %in% colnames(df_by_selections())]
        
        plot_df <- build_scatterplot_df(
            df_by_selections(),
            hm_internal_x(),
            eventdata$x[[1]],
            internal_variable_name,
            input$heatmap_y
        )
        
        plot_df %>% 
            create_gg_scatterplot(
                input$heatmap_y, 
                internal_variable_name,
                get_variable_display_name(input$heatmap_y),
                eventdata$y[[1]],
                eventdata$x[[1]]) %>% 
            create_plotly_scatterplot %>% 
            print
    })
    
    output$violinPlot <- renderPlot({
        
        display_x  <- input$violin_x
        display_y  <- input$violin_y
        internal_x <- get_variable_internal_name(display_x)
        internal_y <- get_variable_internal_name(display_y)

        plot_df <- panimmune_data$df %>% 
            select_(.dots = c(internal_x, internal_y)) %>% 
            .[complete.cases(.),]
        
        plot <- create_violinplot(
            plot_df,
            internal_x,
            internal_y,
            internal_x,
            xlab = display_x,
            ylab = display_y,
            fill_colors = decide_plot_colors(panimmune_data, internal_x)
        ) 
        print(plot)
    })
    
    output$mosaicPlot <- renderPlot({
        
        display_x  <- input$mosaic_x
        display_y  <- input$mosaic_y
        print(display_x)
        print(display_y)
        
        internal_x <- get_variable_internal_name(display_x)
        internal_y <- get_variable_internal_name(display_y)
        print(internal_y)
        print(internal_x)
        
        plot_df <- panimmune_data$df %>% 
            select_(.dots = c(internal_x, internal_y)) %>% 
            .[complete.cases(.),]
        
        print(names(plot_df))
        
        plot <- create_mosaicplot(
            plot_df,
            internal_x,
            internal_y,
            internal_y,
            xlab = display_x,
            ylab = display_y,
            fill_colors = decide_plot_colors(panimmune_data, internal_y)
        ) 
        print(plot)
    })
    
    
}

