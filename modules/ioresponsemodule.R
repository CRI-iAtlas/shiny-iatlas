ioresponse_UI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    titleBox("iAtlas Explorer — Molecular Response to Immune Checkpoint Inhibitors"),
    textBox(
      width = 12,
      p("Explore the ‘omics’ data sets on response to checkpoint inhibitors treatments")
    ),
    
    sectionBox(
      title = "Molecular Response",
      
      messageBox(
        width = 24,
        p("This module generates different analysis of response of immune checkpoint inhibitors (ICI) treatment.")
      ),
      
      optionsBox(
        width=3,
        #verticalLayout(
          # fluidRow(
          #   column(
          #     width = 5,
              checkboxGroupInput(ns("datasets"), "Select Datasets", choices = list("Gide, 2019 - SKCM, Anti-PD1 +/- Anti-CTLA4" =  "Gide 2019", 
                                                                                   "Hugo, 2016 - SKCM, Anti-PD1" = "Hugo 2016", 
                                                                                   "Riaz, 2017 - SKCM, Anti-PD1" = "Riaz 2017", 
                                                                                   "Van Allen, 2015 - SKCM, Anti-CTLA4" = "Van Allen 2015",
                                                                                   "IMVigor210 - BLCA, Anti-PDL1" = "IMVigor210", 
                                                                                   "Prins, 2019 - GBM, Anti-PD1" = "Prins 2019"),
                                 selected = c("Gide 2019", "Hugo 2016")),
              #the Auslander dataset does not have annotation for OS  
          #  ),
          #   column(
          #     width = 7,
          #     selectizeInput(ns("types"), "Select tumor(s) type(s)", choices = c("All", unique(dataset_io_df$Study)), selected = NULL),
          #     selectizeInput(ns("therapy"), "Select therapy(ies) type(s)", choices = c("All", unique(dataset_io_df$Antibody)), selected = NULL),
          #     selectizeInput(ns("drugs"), "Select drug(s) of treatment", choices = c("All", unique(dataset_io_df$Drug)), selected = NULL)
          #   )#column
          #),    
          uiOutput(ns("feature_op")),
         
          selectInput(
            ns("groupvar"),
            "Select Sample Group",
            c("Progression" = "Progression",
              "Response to Treatment" = "Response"),
            selected = "Progression"
          ),
          div(class = "form-group shiny-input-container", actionButton(ns("go_button"), tags$b("GO"), width = "100%"))
      #  )
        
      ),#optionsBox
      column(
        width = 9,
        plotBox(
          width = 12,
          plotlyOutput(ns("dist_plots"), height = "700px") %>%
            shinycssloaders::withSpinner()
        )
        
      )
    ) #sectionBox
  )
}

ioresponse <- function(input, 
                       output, 
                       session, 
                       group_display_choice,
                       group_internal_choice,
                       study_subset_choice,
                       sample_group_df,
                       subset_df,
                       plot_colors){
  
  ns <- session$ns
  
  output$feature_op <- renderUI({
    
    var_choices <- create_filtered_nested_list_by_class(feature_df = feature_io_df,
                                                        filter_value = "Numeric",
                                                        class_column = "Variable Class",
                                                        internal_column = "FeatureMatrixLabelTSV",
                                                        display_column = "FriendlyLabel",
                                                        filter_column = "VariableType")
    selectInput(
      ns("var1_surv"),
      "Variable",
      var_choices
    )
  })
  
  output$dist_plots <- renderPlotly({
    
    all_plots <- purrr::map(.x = input$datasets, function(dataset){
      
      dataset_data <- fmx_io %>% 
        filter(Dataset == dataset) %>% 
        select(Sample_ID, Dataset, input$var1_surv, input$groupvar)
      
      create_violinplot(dataset_data, 
                        x_col = input$groupvar, 
                        y_col = input$var1_surv, 
                        xlab = as.character(input$groupvar),
                        ylab = as.character(input$var1_surv),
                        #yrange = c(0, 9000),
                        fill_colors = c("#0000FF", "#00FF00", "#FF00FF", "#FF0000"),
                        showlegend = F) %>% 
        add_annotations(
          text = dataset,
          x = 0.5,
          y = 1.2,
          yref = "paper",
          xref = "paper",
          xanchor = "center",
          yanchor = "top",
          showarrow = FALSE,
          font = list(size = 15)
        )
                          
    })
    
    plotly::subplot(all_plots, shareY = TRUE, nrows = 2, margin = c(0.02, 0.02, 0.15,0.1))
    
   # dataset_data %>% 
   #      ggplot(aes_string(y = input$var1_surv, x =input$groupvar))+
   #        geom_violin()+
   #        theme_bw()+
   #        facet_wrap( ~ Dataset)
  })
    
}