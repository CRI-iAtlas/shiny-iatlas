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
                uiOutput(ns("heatmap_op")),
                selectInput(
                  ns("timevar"),
                  "Survival Endpoint",
                  c("Overall Survival" = "OS_time",
                    "Progression Free Interval" = "PFI_time_1"),
                  selected = "OS_time"
                )
                ),
            conditionalPanel(condition = paste0("input['", ns("timevar"), "'] == 'PFI_time_1'"),
                             helpText("There is no PFI annotation for Hugo 2016, Riaz 2017, and IMVigor210.")),
            plotBox(
                width = 10,
                plotlyOutput(ns("mult_forest"), width = "100%", height = "600px")%>%
                    shinycssloaders::withSpinner(),
                DT::dataTableOutput(ns("stats_summary"))
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

      clin_data <- ioresponse_data$feature_df %>%
        dplyr::filter(`Variable Class` %in% c("Immune Checkpoint Treatment - Study Condition"))

      var_choices_clin <- create_filtered_nested_list_by_class(feature_df = clin_data,
                                                          filter_value = "Categorical",
                                                          class_column = "Variable Class",
                                                          internal_column = "FeatureMatrixLabelTSV",
                                                          display_column = "FriendlyLabel",
      filter_column = "VariableType")
        
      var_choices_feat <- create_filtered_nested_list_by_class(feature_df = ioresponse_data$feature_df,
                                                            filter_value = "Numeric",
                                                            class_column = "Variable Class",
                                                            internal_column = "FeatureMatrixLabelTSV",
                                                            display_column = "FriendlyLabel",
                                                            filter_column = "VariableType")
      var_choices <- c(var_choices_clin, var_choices_feat)
      
        selectizeInput(
            ns("var2_cox"),
            "Select features",
            var_choices,
            #selected = var_choices$`Predictor - Immune Checkpoint Treatment`,
            multiple = TRUE
        )
    })
    
    datasets <- reactive({
      switch(
        input$timevar,
        "OS_time" = input$datasets_mult,
        "PFI_time_1"= input$datasets_mult[input$datasets_mult %in% datasets_PFI]
      )
    })

    feature_df_mult <- reactive({
        
      req(input$datasets_mult, input$var2_cox)
      
      if (input$timevar == "OS_time") {
        status_column <- "OS"
      } else {
        status_column <- "PFI_1"
      }
        
      ioresponse_data$fmx_df %>% 
        filter(Dataset %in% datasets()) %>% 
        `if`(
          !("treatment_when_collected" %in% input$var2_cox),
          dplyr::filter(., treatment_when_collected == "Pre"),
          .
        ) %>% 
            #filter(Dataset %in% input$datasets_mult & treatment_when_collected == "Pre") %>%
            select(Sample_ID, Dataset, input$timevar, status_column, treatment_when_collected, dplyr::one_of(input$var2_cox))
    })
    
    #create dataframe with cox proportional hazard ratio for the selected datasets
    
    #1.list which features have more than one level for each selected dataset
    
    dataset_ft <- reactive({
      req(input$datasets_mult, input$var2_cox)
      
      all_comb <- tidyr::crossing(dataset = datasets(), feature = input$var2_cox) %>% 
        merge(., ioresponse_data$feature_df %>% dplyr::select(FeatureMatrixLabelTSV, FriendlyLabel,VariableType, `Variable Class Order`), 
              by.x = "feature", by.y ="FeatureMatrixLabelTSV")
      
      num_cols <- all_comb[which(all_comb$VariableType == "Numeric"),]
      cat_cols <- all_comb[which(all_comb$VariableType == "Categorical"),]
      
      if(nrow(num_cols)>0){
        num_cols <- num_cols %>%
          dplyr::select(dataset,
                        group = feature,
                        group_label = FriendlyLabel,
                        order_within_sample_group = `Variable Class Order`) %>% 
          dplyr::mutate(feature="Immune Feature",
                        ft_label = "Immune Feature")
         
      }
      
      if(nrow(cat_cols)>0){
        cat_values <- purrr::map2_dfr(.x = cat_cols$dataset, .y = cat_cols$feature, .f = function(x, y){
          
          uvalue <- unique((feature_df_mult() %>% 
                          dplyr::filter(Dataset == x))[[y]]) 
       
          if(length(uvalue)>1) data.frame(dataset = x, feature = y, gname = uvalue) %>% mutate(group = paste0(y, gname))
          else return()
        })
        
      cat_cols <- merge(cat_values, cat_cols, by = c("dataset", "feature")) %>% 
        merge(., ioresponse_data$sample_group_df %>% dplyr::select(FeatureValue, FeatureName, order_within_sample_group), 
              by.x = "gname", by.y = "FeatureValue") %>% 
        select(dataset, feature, ft_label = FriendlyLabel, group, group_label = FeatureName, order_within_sample_group)
      }
      rbind(cat_cols, num_cols)
    })
    
    mult_ph_df <- reactive({
        req(input$datasets_mult, input$var2_cox)
      
      if (input$timevar == "OS_time") {
        status_column <- "OS"
      } else {
        status_column <- "PFI_1"
      }
      
        fit_cox <- function(dataset, data){
            #filtering data to dataset level
            data_cox <- data %>% 
                filter(Dataset == dataset)
            
            #checking which features have more than one level for the dataset
            valid_ft <- purrr::map(input$var2_cox, .f= function(x){
              if(dplyr::n_distinct(data_cox[[x]])>1) return(x)
              else return()
            })
            
            cox_features <- as.formula(paste(
              "survival::Surv(", input$timevar, ",", status_column, ") ~ ", 
              paste0(valid_ft, collapse = " + ")
            ))
            
            survival::coxph(cox_features, data_cox)
        }
        
        all_hr <- purrr::map(.x = datasets(), .f= fit_cox, data = feature_df_mult())
        names(all_hr) <- datasets()
        
        create_ph_df <- function(coxphList){
            
            coef_stats <- as.data.frame(summary(coxphList)$conf.int)
            coef_stats$group <- row.names(coef_stats)
            coef_stats$pvalue <- (coef(summary(coxphList))[,5])
            coef_stats$logpvalue <- -log10(coef(summary(coxphList))[,5])
            coef_stats
        }
        
        cox_coef <- purrr::map(all_hr, create_ph_df)
       
        data.table::rbindlist(cox_coef, fill = T, idcol = TRUE) %>% 
            dplyr::mutate(logHR = log10(`exp(coef)`),
                   logupper = log10(`upper .95`),
                   loglower = log10(`lower .95`),
                   difflog=logHR-loglower) %>% 
          dplyr::rename(dataset = ".id")
    })
    
    mult_label_df <- reactive({
      suppressMessages(dplyr::right_join(mult_ph_df(), dataset_ft())) %>% 
      dplyr::mutate(group_label=replace(group_label, is.na(logHR), paste("(Ref.)", .$group_label[is.na(logHR)]))) %>% 
      dplyr::mutate_all(~replace(., is.na(.), 0))
    })
    
    
    output$mult_forest <- renderPlotly({
      shiny::validate(need(!is.null(input$datasets_mult), "Select at least one dataset."))
      # print(mult_ph_df())
      # mult_label_df <- suppressMessages(dplyr::right_join(mult_ph_df(), dataset_ft())) %>% 
      #                  dplyr::mutate(group_label=replace(group_label, is.na(logHR), paste("(Ref.)", .$group_label[is.na(logHR)]))) %>% 
      #                  dplyr::mutate_all(~replace(., is.na(.), 0))
      # 
      all_forests <- purrr::map(.x = unique(mult_ph_df()$dataset), function(x){
        
        subset_df <- mult_label_df() %>% 
          dplyr::filter(dataset == x) 
        
        #ordering sample groups when more than one level is selected
        #if(dplyr::n_distinct(subset_df$feature)>1){
          subset_df <- subset_df %>%
            #dplyr::arrange(logpvalue, feature, (abs(logHR)))
            dplyr::arrange(feature, desc(abs(logHR)))

          subset_df$group_label <- factor(subset_df$group_label, levels = subset_df$group_label)
        #}
        
        p <-  create_forestplot_plotly(x = subset_df$logHR,
                                 y = subset_df$group_label,
                                 error = subset_df$difflog,
                                 p.values = subset_df$logpvalue,
                                 #dataset = subset_df$dataset,
                                 xlab = "Hazard Ratio (log10)") %>%
          add_annotations(
            text = x,
            x = 0.5,
            y = 1,
            yref = "paper",
            xref = "paper",
            xanchor = "center",
            yanchor = "top",
            yshift = 30,
            showarrow = FALSE,
            font = list(size = 15)) 
        
        
        if(dplyr::n_distinct(subset_df$feature)>1){
          p <- p %>% 
            layout(
              shapes = lazyeval::lazy_eval(get_hlines_pos(subset_df %>% dplyr::select(var1 = feature)))
            )
          }
        p
      })
      npannel <- ((dplyr::n_distinct(mult_ph_df()$dataset)+1)%/%2)
      print(mult_label_df())
      plotly::subplot(all_forests, nrows = npannel, titleX = TRUE, titleY = TRUE, margin = 0.09)
      
      
    })
    
    output$stats_summary <- DT::renderDataTable(
      
      mult_label_df() %>% 
        dplyr::select(dataset, Feature = ft_label, Variable = group_label, `log10(HR)` = logHR, loglower, logupper, pvalue, '-log10(p.value)' = logpvalue) %>%
        dplyr::mutate_if(is.numeric, formatC, digits = 3) %>%
        dplyr::arrange(dataset)
    )
    
    output$mult_heatmap <- renderPlotly({

        heatmap_df <- mult_ph_df() %>%
            dplyr::select(dataset, group, logHR) %>%
            tidyr::spread(key = group, value = logHR)
     
        row.names(heatmap_df) <- heatmap_df$dataset
        heatmap_df$dataset <- NULL
        
        # colnames(heatmap_df) <- convert_value_between_columns(input_value =colnames(heatmap_df),
        #                                                       df = ioresponse_data$feature_df,
        #                                                       from_column = "FeatureMatrixLabelTSV",
        #                                                       to_column = "FriendlyLabel",
        #                                                       many_matches = "return_result")

        p <- create_heatmap(t(as.matrix(heatmap_df)), "heatmap", scale_colors = T, legend_title = "Hazard Ratio (log10)")
        
        p #+ geom_tile(size = 1, colour = "black")
    })
}