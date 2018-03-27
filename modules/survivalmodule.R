survival_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    titleBox("Immune Feature Kaplan-Meier Plot"),
    fluidRow(
      optionsBox(width = 4,
        selectInput(
          ns("var1_surv"),
          "Variable",
          c(
            "Immune Subtypes" = "Subtype_Immune_Model_Based",
            "Leukocyte Fraction" = "leukocyte_fraction",
            "Mutation Rate, Non-Silent" = "mutationrate_nonsilent_per_Mb",
            "Indel Neoantigens" = "indel_neoantigen_num",
            "SNV Neoantigens" = "numberOfImmunogenicMutation",
            "Stemness Score RNA" = "StemnessScoreRNA"
          ),
          selected = "Subtype_Immune_Model_Based"
        ),
        
        selectInput(
          ns("timevar"),
          "Time Varible",
          c("OS Time" = "OS_time", "PFI Time" = "PFI_time_1"),
          selected = "OS_time"
        ),
        
        sliderInput(
          ns("divk"),
          "Divider",
          min = 0,
          max = 10,
          value = 2
        ),
        
        checkboxInput(ns("confint"), "Confidence Intervals", value = F),
        checkboxInput(ns("risktable"), "Risk Table", value = T)
      ),
      plotBox(width = 8,
        plotOutput(ns("survPlot"), height = 600)
      )
    )
  )
}

survival <- function(input, output, session) {
  output$survPlot <- renderPlot({
    df <- build_survival_df(panimmune_data$df, input$var1_surv, input$timevar, input$divk)
    fit <- survfit(Surv(Time, Status) ~ Variable, data = df)
    title <- get_variable_display_name(input$var1_surv)
    create_kmplot(fit, df, input$confint, input$risktable, title)
  })
}
