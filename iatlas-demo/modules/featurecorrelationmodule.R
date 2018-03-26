featurecorrelation_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(width = 12, background = "black",
          span(strong("Immune Feature Correlation Heatmap"),
               style = "font-size:18px")
      )
    ),
    fluidRow(
      box(width = 4,
        selectInput(
          ns("var1"),
          "Variable 1",
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
          ns("var2"),
          "Variable 2",
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
          ns("catx"),
          "Category",
          c(
            "Immune Subtypes" = "Subtype_Immune_Model_Based",
            "TCGA Tissues" = "Study",
            "TCGA Subtypes" = "Subtype_Curated_Malta_Noushmehr_et_al"
          ),
          selected = "Immune Subtypes"
        )
      ),
      
      box(width = 8,
        plotlyOutput(ns("corrPlot")),
        plotlyOutput(ns("scatterPlot")),
        HTML("<br><br><br>")
      )
    )
  )
}

featurecorrelation <- function(input, output, session) {
  categories <- reactive(get_category_group(input$catx))
  variables <- reactive(as.character(get_variable_group(input$var1)))
  
  df_by_selections <- reactive(filter_data_by_selections(
    input$var2,
    input$catx,
    categories(),
    variables()
  ))
  
  output$corrPlot <- renderPlotly({
    corr_matrix <- build_correlation_mat(
      df_by_selections(),
      input$var2,
      input$catx,
      categories(),
      variables()
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
      input$catx,
      eventdata$x[[1]],
      internal_variable_name,
      input$var2
    )
    
    plot_df %>% 
        create_gg_scatterplot(
            input$var2, 
            internal_variable_name,
            get_variable_display_name(input$var2),
            eventdata$y[[1]],
            eventdata$x[[1]]) %>% 
        create_plotly_scatterplot %>% 
        print
  })
}

