# UI ----
survival_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    titleBox("Clinical Outcomes"),
    textBox(
      width = 12,
      p("Plot survival curves based on immune characteristics and identify variables associated with outcome.")  
    ),
    
    # Survival comparison section ----
    sectionBox(
      title = "Sample Group Survival",
      messageBox(
        width = 12,
        p("Select variable, and survival either as overall survival (OS) or progression-free interval (PFI) to get a Kaplan-Meier plot. For a continuous (numeric) variable the slider can be used to select quantile for dichotomizing data."),
        p("For immune subtypes Figure 3A can be generated (OS), and Figure S3A for (PFI).")
      ),
      fluidRow(
        optionsBox(
          width = 4,
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
        
        # ** Survival Kaplan-Meier plot ----
        plotBox(
          width = 8,
          plotOutput(ns("survPlot"), height = 600)
        )
      )
    ),
    
    # Survival comparison section ----
    sectionBox(
      title = "Concordance Index",
      messageBox(
        width = 12,
        p("For your sample groups, you can explore which variables correlate with improved or lessened survival. Select a variable class, and you will get a heatmap. Red denotes decreased survival, and blue increased survival as the variable is increased."),
        p("Manuscript context:  Selecting variable class “Core Expression Signature”, you can generate Figure 3B. Figures 3C, and Figures S3B, S3C, and S3C can also be generated with different selection options.")
      )
    )
  )
}

# Server ----
survival <- function(input, output, session, ss_choice) {
  output$survPlot <- renderPlot({
    
      survival_df <- panimmune_data$fmx_df %>% 
        build_survival_df(
          group_column = input$var1_surv, 
          time_column = input$timevar, 
          k = input$divk
        )
    
    fit <- survfit(Surv(time, status) ~ variable, data = survival_df)
    title <- get_variable_display_name(input$var1_surv)
    
    create_kmplot(fit, survival_df, input$confint, input$risktable, title)
  })
}
