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
            ns("violin_y"),
            "Select violin plot y variable",
            choices = get_friendly_numeric_columns(),
            selected = "leukocyte_fraction" 
        ),
        
        selectInput(
            ns("mosaic_y"),
            "Select mosaic y variable",
            as.character(
                panimmune_data$sample_selection_choices),
            selected = "TCGA Study"
        )

      ),
      
      plotBox(
          width = 8,
          plotlyOutput(ns("corrPlot")),
          plotlyOutput(ns("scatterPlot")),
          plotOutput(ns("violinPlot")),
          plotOutput(ns("mosaicPlot")),
          HTML("<br><br><br>")
      )
    )
  )
}

featurecorrelation <- function(input, output, session, ss_choice, subset_df) {
    
    hm_display_x  <- reactive(ss_choice())
    hm_internal_x <- reactive(get_variable_internal_name(hm_display_x()))
    hm_categories <- reactive(get_category_group(hm_internal_x()))
    hm_variables  <- reactive(as.character(get_variable_group(input$heatmap_values)))
    
    
    intermediate_corr_df <- reactive(create_intermediate_corr_df(
        subset_df(),
        input$heatmap_y,
        hm_internal_x(),
        hm_categories(),
        hm_variables()
    ))
    
    output$corrPlot <- renderPlotly({
        heatmap_corr_matrix <- create_heatmap_corr_matrix(
            intermediate_corr_df(),
            input$heatmap_y,
            hm_internal_x(),
            hm_categories(),
            hm_variables()
        )
        
        heatmap_plot <- create_plotly_heatmap(heatmap_corr_matrix)
    })
    
    output$scatterPlot <- renderPlotly({
        eventdata <- event_data("plotly_click", source = "heatplot")
        validate(need(!is.null(eventdata), "Click heatmap"))
        
        internal_variable_name <-
            eventdata$y[[1]] %>%
            get_variable_internal_name() %>%
            .[. %in% colnames(intermediate_corr_df())]

        plot_df <- create_scatterplot_df(
            intermediate_corr_df(),
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
        
        display_x  <- ss_choice()
        display_y  <- input$violin_y
        internal_x <- get_variable_internal_name(display_x)
        internal_y <- get_variable_internal_name(display_y)

        plot_df <- subset_df() %>% 
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
        
        display_x  <- ss_choice()
        display_y  <- input$mosaic_y
        
        internal_x <- get_variable_internal_name(display_x)
        internal_y <- get_variable_internal_name(display_y)
        
        plot_df <- let(
            alias = c(COLX = internal_x,
                      COLY = internal_y),
            subset_df() %>% 
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
        print(plot)
    })
    
    
}

