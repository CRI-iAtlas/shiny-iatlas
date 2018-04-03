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
        p("Select variable, and outcome in terms of either overall survival (OS) or progression-free interval (PFI) to generate a Kaplan-Meier plot. For a continuous (numeric) variable the slider can be used to select to designate splits. The value 2 corresponds to a median split to dichotomize the data, 3 to tertiles for three sample categories, 4 to quartiles, 5 to quintiles and so on."),
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
          plotOutput(ns("survPlot"), height = 600) %>% 
            shinycssloaders::withSpinner()
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
      ),
      fluidRow(
          optionsBox(
              width = 4,
              radioButtons(
                  ns("survival_type"), 
                  "Select survival type",
                  c("Progression-Free Interval" = "PFI",
                    "Overall Survival" = "OS"
                  ),
                  selected = "PFI"
              ),
              selectInput(
                  ns("survival_class"),
                  "Select concordance variables class",
                  choices = get_numeric_variable_classes(),
                  selected = "T Helper Cell Score"
              )
          ),
          plotBox(
              plotlyOutput(ns("heatmapplot"), height = 600) %>%
                  shinycssloaders::withSpinner()
          )
      )
    )
  )
}

# Server ----
survival <- function(input, output, session, ss_choice, subset_df) {
  output$survPlot <- renderPlot({
    
      survival_df <- panimmune_data$fmx_df %>% 
        build_survival_df(
          group_column = input$var1_surv, 
          time_column = input$timevar, 
          k = input$divk
        )
    
    fit <- survival::survfit(Surv(time, status) ~ variable, data = survival_df)
    title <- get_variable_display_name(input$var1_surv)
    
    create_kmplot(fit, survival_df, input$confint, input$risktable, title)
  })
  
  
  
  output$heatmapplot <- renderPlotly({
      # features <- as.character(get_variable_group("T Helper Cell Score"))
      # group_internal <- "Subtype_Immune_Model_Based"
      # time_col <- "OS_time"
      # status_col <- "OS"
      # subset_df <- panimmune_data$fmx_df
      if(input$survival_type == "PFI"){
          time_col <- "OS_time"
          status_col <- "OS"
      } else{
          time_col <- "PFI_time_1"
          status_col <- "PFI_1"
      }
      
      features <- as.character(get_variable_group(input$survival_class))
      group_internal <- get_variable_internal_name(ss_choice())
      
      ci_mat <- subset_df() %>% 
        build_ci_mat(
          group_column = group_internal, 
          value_columns = features, 
          time_column = time_col, 
          status_column = status_col
        )
      create_heatmap(ci_mat, "ci")
  })
  
}


