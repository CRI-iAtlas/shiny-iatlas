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
              checkboxGroupInput(ns("datasets"), "Select Datasets", choices = list("Auslander, 2018 - SKCM" =  "Auslander 2018", 
                                                                                   "Gide, 2019 - SKCM, Anti-PD1 +/- Anti-CTLA4" =  "Gide 2019", 
                                                                                   "Hugo, 2016 - SKCM, Anti-PD1" = "Hugo 2016", 
                                                                                   "Riaz, 2017 - SKCM, Anti-PD1" = "Riaz 2017", 
                                                                                   "Van Allen, 2015 - SKCM, Anti-CTLA4" = "Van Allen 2015",
                                                                                   "IMVigor210 - BLCA, Anti-PDL1" = "IMVigor210", 
                                                                                   "Prins, 2019 - GBM, Anti-PD1" = "Prins 2019"),
                                 selected = c("Gide 2019", "Hugo 2016")),
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
      fluidRow(
        width = 12,
        plotBox(
          width = 12,
          plotlyOutput(ns("dist_plots"), height = "auto") %>%
            shinycssloaders::withSpinner()
        )
      )
      ),
    optionsBox(
      width = 12,
   
        uiOutput(ns("extra_features"))
      
    ),
      fluidRow(
        width = 12,
        plotBox(
          width = 12,
          plotlyOutput(ns("dist_plots2"), height = "auto") %>%
            shinycssloaders::withSpinner()
        )
        
      )
      
   # ) #sectionBox
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
  
  output$extra_features <- renderUI({
      sel2 <- lapply(input$datasets, function(id){
        clin_data <- feature_io_df %>% 
          dplyr::filter(`Variable Class` == paste("Clinical data for", id) | `Variable Class` == "Immune Checkpoint Treatment")
        
        var_choices <- create_filtered_nested_list_by_class(feature_df = clin_data,
                                                            filter_value = "Categorical",
                                                            class_column = "Variable Class",
                                                            internal_column = "FeatureMatrixLabelTSV",
                                                            display_column = "FriendlyLabel",
                                                            filter_column = "VariableType")
        selectInput(
          ns(paste0("dist", id)),
          label = paste("Select extra group for", as.character(id)),
          choices = var_choices #list(split(clin_data$FeatureMatrixLabelTSV, clin_data$FriendlyLabel), "Drug" = "Drug", "Treatment" = "treatment_when_collected")
        )
      })
      do.call(flowLayout, sel2)
  })
  
  
  output$dist_plots <- renderPlotly({
    
    all_plots <- purrr::map(.x = input$datasets, function(dataset){
      
      dataset_data <- fmx_io %>% 
        filter(Dataset == dataset) %>% 
        select(Sample_ID, Dataset, input$var1_surv, input$groupvar)
      
      ylabel <-  convert_value_between_columns(input_value =input$var1_surv,
                                               df = feature_io_df,
                                               from_column = "FeatureMatrixLabelTSV",
                                               to_column = "FriendlyLabel")
  
      create_violinplot(dataset_data, 
                        x_col = input$groupvar, 
                        y_col = input$var1_surv, 
                        xlab = (input$groupvar),
                        ylab = ylabel,
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
    
    plotly::subplot(all_plots, shareX = TRUE, shareY = TRUE, nrows = 1, margin = c(0.01, 0.01, 0.01,0.01))
    
    #titleX = TRUE, 
   # dataset_data %>% 
   #      ggplot(aes_string(y = input$var1_surv, x =input$groupvar))+
   #        geom_violin()+
   #        theme_bw()+
   #        facet_wrap( ~ Dataset)
  })
  
  output$dist_plots2 <- renderPlotly({
    
    all_plots <- purrr::map(.x = input$datasets, function(dataset){
      
      id <- paste0("dist", dataset)
      dataset_data <- fmx_io %>% 
        filter(Dataset == dataset) %>% 
        select(Sample_ID, Dataset, input$var1_surv, input$groupvar, input[[id]], )
      
      
      dataset_data$Comb_feat <- paste(dataset_data[[input$groupvar]], dataset_data[[input[[id]]]], sep = "_" )
     
      ylabel <-  convert_value_between_columns(input_value =input$var1_surv,
                                               df = feature_io_df,
                                               from_column = "FeatureMatrixLabelTSV",
                                               to_column = "FriendlyLabel")
      # from: https://community.plot.ly/t/how-to-plot-multiple-x-axis-in-plotly-in-r/3014/3
      # ax2 <- list(
      #   title = " ",
      #   overlaying = "x",
      #   anchor = "y",
      #   side = "top",
      #   showticklabels = TRUE,
      #   tickangle = 0
      # )
      
      create_violinplot(dataset_data, 
                        x_col = input$groupvar, 
                        y_col = input$var1_surv, 
                        xlab = as.factor(input$groupvar),
                        ylab = ylabel,
                        color = (as.character(input[[id]])),
                        #yrange = c(0, 9000),
                        fill_colors = c("#0000FF", "#00FF00", "#FF00FF", "#FF0000"),
                        #legendgroup = as.character(input[[id]]),
                        showlegend = T) %>% 
        add_annotations(
          text = dataset,
          x = 0.5,
          y = 1.2,
          yref = "paper",
          xref = "paper",
          xanchor = "center",
          yanchor = "top",
          showarrow = FALSE,
          font = list(size = 15)) %>%
        layout(violingap = 0,
               violingroupgap = 0.1,
               violinmode = "group",
               legend = list(orientation = 'h'))
        
    })
    #TODO: legend for each plot, see: https://stackoverflow.com/questions/51287107/legend-near-each-plot-in-subplot-plot-ly-in-r
    
    plotly::subplot(all_plots, shareX = TRUE, shareY = TRUE, nrows = 1, margin = c(0.01, 0.01, 0.01,0.01))  
  })
    
  
}