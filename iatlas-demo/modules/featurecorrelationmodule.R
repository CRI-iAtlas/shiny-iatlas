featurecorrelation_UI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidPage(
      titlePanel("Immune Feature Correlation Heatmap"),
      sidebarLayout(
        sidebarPanel(
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

        mainPanel(
          plotlyOutput(ns("corrPlot")),
          plotlyOutput(ns("scatterPlot")),
          HTML("<br><br><br>")
        )
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
    corr_matrix <- create_correlation_matrix(
      df_by_selections(),
      input$var2,
      input$catx,
      categories(),
      variables()
    )


    plot_ly(
      z = corr_matrix,
      x = colnames(corr_matrix),
      y = rownames(corr_matrix),
      type = "heatmap",
      source = "heatplot",
      colors = colorRamp(c("blue", "white", "red"))
    ) %>%
      layout(margin = list(
        l = 100,
        r = 10,
        b = 50,
        t = 10,
        pad = 2
      ))
  })

  output$scatterPlot <- renderPlotly({
    eventdata <- event_data("plotly_click", source = "heatplot")
    validate(need(!is.null(eventdata), "Click heatmap"))

    internal_variable_name <-
      eventdata$y[[1]] %>%
      get_variable_internal_name() %>%
      .[. %in% colnames(df_by_selections())]

    plot_df <- create_scatter_plot_df(
      df_by_selections(),
      input$catx,
      eventdata$x[[1]],
      internal_variable_name,
      input$var2
    )

    plot <- plot_df %>%
      ggplot(aes_string(x = input$var2, y = internal_variable_name)) +
      geom_point() +
      theme_bw() +
      theme_1012 +
      xlab(get_variable_display_name(input$var2)) +
      ylab(eventdata$y[[1]]) +
      labs(title = eventdata$x[[1]])



    plot %>%
      ggplotly() %>%
      layout(margin = list(
        l = 100,
        r = 40,
        b = 50,
        t = 100,
        pad = 2
      )) %>%
      print()
  })
}

create_scatter_plot_df <- function(
                                   df,
                                   category_column,
                                   category_plot_selection,
                                   internal_variable_name,
                                   variable2_selection) {
  plot_df <- df %>%
    filter(UQ(as.name(category_column)) == category_plot_selection) %>%
    select_(.dots = variable2_selection, internal_variable_name)
}
