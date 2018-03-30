# UI ----
survival_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    titleBox("Clinical Outcomes"),
    textBox(
      width = 12,
      p("Some overview/summary text describing this module and the data presented within.")  
    ),
    
    # Survival comparison section ----
    sectionBox(
      title = "Sample Group Survival",
      messageBox(
        width = 12,
        p("Brief instructional message about this section, what to do in it, and the available options.")  
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
    )
  )
}

survival <- function(input, output, session, ss_choice) {
  output$survPlot <- renderPlot({
    
      survival_df <- panimmune_data$fmx_df %>% 
        build_survival_df(
          group_column = input$var1_surv, 
          time_column = input$timevar, 
          k = input$divk
        )
    
    fit <- survfit(Surv(Time, Status) ~ Variable, data = survival_df)
    title <- get_variable_display_name(input$var1_surv)
    
    create_kmplot(fit, survival_df, input$confint, input$risktable, title)
  })
}
