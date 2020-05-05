ioresponsemultivariate_UI <- function(id){
    
    ns <- NS(id)
    
    tagList(
        titleBox("iAtlas Explorer —  Multivariate survival analysis to Immune Checkpoint Inhibitors"),
        textBox(
            width = 12,
            p("Create multivariate survival analysis and visualize Hazard Ratio in a forest plot and a heatmap.")
        ),
        
        sectionBox(
            title = "Multivariate analysis",

            messageBox(
                width = 24,
                p("Select the datasets of interest, variables, and outcome in terms of either overall survival (OS) or progression free interval (PFI) 
                  endpoints to generate a forest plot with the log10 of the Cox Proportional Hazard Ratio with 95th confidence intervals for each variable. 
                  A heatmap with the log10 of Hazard Ratio for all the datasets and variables selected is also displayed. ")
            ),
            
            optionsBox(
                width = 2, 
                checkboxGroupInput(ns("datasets_mult"), "Select Datasets", choices = datasets_options,
                                   selected =  c("Gide 2019", "Hugo 2016", "Riaz 2017", "Van Allen 2015")), 
                uiOutput(ns("heatmap_op"))
                ),
            
            plotBox(
                width = 10,
                plotlyOutput(ns("mult_forest"), width = "100%", height = "600px")%>%
                    shinycssloaders::withSpinner()
            ),
            plotBox(
                width = 12,
                plotlyOutput(ns("mult_heatmap"), width = "100%", height = "500px")%>%
                    shinycssloaders::withSpinner()
            )
        )
    )
}

ioresponsemultivariate <- function(input, 
                       output, 
                       session){
    
    ns <- session$ns
    
    output$heatmap_op <- renderUI({
      # 
      # clin_data <- ioresponse_data$feature_df %>% 
      #   dplyr::filter(`Variable Class` %in% c("Immune Checkpoint Treatment - Study Condition"))
      # 
      # var_choices_clin <- create_filtered_nested_list_by_class(feature_df = clin_data,
      #                                                     filter_value = "Categorical",
      #                                                     class_column = "Variable Class",
      #                                                     internal_column = "FeatureMatrixLabelTSV",
      #                                                     display_column = "FriendlyLabel",
                                                          # filter_column = "VariableType")
        
      var_choices <- create_filtered_nested_list_by_class(feature_df = ioresponse_data$feature_df,
                                                            filter_value = "Numeric",
                                                            class_column = "Variable Class",
                                                            internal_column = "FeatureMatrixLabelTSV",
                                                            display_column = "FriendlyLabel",
                                                            filter_column = "VariableType")
      #var_choices <- c(var_choices_clin, var_choices_feat)
      
        selectizeInput(
            ns("var2_cox"),
            "Select features",
            var_choices,
            #selected = var_choices$`Predictor - Immune Checkpoint Treatment`,
            multiple = TRUE
        )
    })

    feature_df_mult <- reactive({
        
        req(input$datasets_mult, input$var2_cox)
        
      ioresponse_data$fmx_df %>% 
            filter(Dataset %in% input$datasets_mult & treatment_when_collected == "Pre") %>%
            select(Sample_ID, Dataset, OS, OS_time, treatment_when_collected, dplyr::one_of(input$var2_cox))
    })
    
    #create dataframe with cox proportional hazard ratio for the selected datasets
    
    mult_ph_df <- reactive({
        req(input$datasets_mult, input$var2_cox)
        
        cox_features <- as.formula(paste(
            "survival::Surv(OS_time, OS) ~", 
            paste0(input$var2_cox, collapse = " + ")
        ))
        
        fit_cox <- function(dataset, data){
            
            data_cox <- data %>% 
                filter(Dataset == dataset)
            
            ph <- survival::coxph(cox_features, data_cox)
            #p <- survminer::ggforest(ph, data = data_cox , main = dataset, cpositions = c(0.02, 0.82, 1))
            ph
        }
        
        all_hr <- purrr::map(.x = input$datasets_mult, .f= fit_cox, data = feature_df_mult())
        names(all_hr) <- input$datasets_mult
        
        create_ph_df <- function(coxphList){
            
            coef_stats <- as.data.frame(summary(coxphList)$conf.int)
            coef_stats$feature <- row.names(coef_stats)
            coef_stats
        }
        
        cox_coef <- purrr::map(all_hr, create_ph_df)
        cox_df <- data.table::rbindlist(cox_coef, idcol = TRUE)
        
        cox_df <- cox_df %>% 
            mutate(logHR = log10(`exp(coef)`),
                   logupper = log10(`upper .95`),
                   loglower = log10(`lower .95`))
        
        cox_df
    })
    
    output$mult_forest <- renderPlotly({
            
        ph_labels<- convert_value_between_columns(input_value =mult_ph_df()$feature,
                                                  df = ioresponse_data$feature_df,
                                                  from_column = "FeatureMatrixLabelTSV",
                                                  to_column = "FriendlyLabel",
                                                  many_matches = "return_result")
        print(mult_ph_df())
            create_forestplot(mult_ph_df(),
                              x=mult_ph_df()$logHR, 
                              y=rep(ph_labels, times = length(input$datasets_mult)),#rep(mult_ph_df()$feature, times = length(input$datasets_mult)), 
                              xmin=mult_ph_df()$loglower, 
                              xmax=mult_ph_df()$logupper, 
                              xintercept = 0, 
                              title = "",
                              xlab = 'Hazard Ratio (log10)',
                              facet = ".id")
    })
    
    output$mult_heatmap <- renderPlotly({

        heatmap_df <- mult_ph_df() %>%
            dplyr::select(.id, feature, logHR) %>%
            tidyr::spread(key = feature, value = logHR)
      
        row.names(heatmap_df) <- heatmap_df$.id
        heatmap_df$.id <- NULL
        colnames(heatmap_df) <- convert_value_between_columns(input_value =colnames(heatmap_df),
                                                              df = ioresponse_data$feature_df,
                                                              from_column = "FeatureMatrixLabelTSV",
                                                              to_column = "FriendlyLabel",
                                                              many_matches = "return_result")

        p <- create_heatmap(t(as.matrix(heatmap_df)), "heatmap", scale_colors = T, legend_title = "Hazard Ratio (log10)")
        
        p #+ geom_tile(size = 1, colour = "black")
    })
}