iosurvival_UI <- function(id){
    
    ns <- NS(id)
     
    tagList(
        titleBox("iAtlas Explorer — Clinical Outcomes to Immune Checkpoint Inhibitors"),
        textBox(
            width = 12,
            p("Plot survival curves based on immune characteristics and identify variables associated with outcome.")
        ),
        
        sectionBox(
            title = "Clinical Outcomes",
            
            messageBox(
                width = 24,
                p("Select the datasets of interest, variable, and outcome in terms of either overall survival (OS) or progression free interval (PFI) endpoints to generate a Kaplan-Meier plot. 
                  For a continuous (numeric) variable, the range can be split in the median of the interval, or into equal intervals of the value range. 
                  For the latter, the slider can be used to specify how the range of values of that variable is split. Selecting 2 splits the values by the middle of the range, 3 splits the range into three even intervals and so on.")
            ),
            
            optionsBox(
                width=3,
                verticalLayout(
                        fluidRow(
                            column(
                                width = 12,
                                checkboxGroupInput(ns("datasets"), "Select Datasets", choices = datasets_options,
                                                   selected =  c("Gide 2019", "Hugo 2016"))
                            )
                        ),    
                        uiOutput(ns("survplot_op")),
                        checkboxInput(ns("confint"), "Confidence Intervals", value = F),
                        checkboxInput(ns("risktable"), "Risk Table", value = T),
                   
                        selectInput(
                            ns("timevar"),
                            "Survival Endpoint",
                            c("Overall Survival" = "OS_time",
                              "Progression Free Interval" = "PFI_time_1"),
                            selected = "OS_time"
                        ),
                        radioButtons(ns("div_range"), "Divide value range", 
                                     choices = c("In the median" = "median", "In equal intervals" = "intervals"), 
                                     inline = TRUE, selected = "median"),
                        conditionalPanel(condition = paste0("input['", ns("div_range"), "'] == 'intervals'"),
                                         sliderInput(
                                           ns("divk"),
                                           "Value Range Divisions",
                                           min = 2,
                                           max = 10,
                                           value = 2
                                         ))
                )
            ),#optionsBox
            column(
                width = 9,
                conditionalPanel(condition = paste0("input['", ns("timevar"), "'] == 'PFI_time_1'"),
                                 helpText("There is no PFI annotation for Hugo 2016, Riaz 2017, and IMVigor210.")),
                uiOutput(ns("notification")),
                plotBox(
                    width = 12,
                    uiOutput(ns("plots")) %>%
                      shinycssloaders::withSpinner()
                )
            )
        ) #sectionBox
    )
}

iosurvival <- function(input, 
                       output, 
                       session){
    
    ns <- session$ns
    
    output$survplot_op <- renderUI({
      
      clin_data <- ioresponse_data$feature_df %>%
        dplyr::filter(FeatureMatrixLabelTSV != "treatment_when_collected" & FeatureMatrixLabelTSV %in% ioresponse_data$categories_df$Category)
      
      var_choices_clin <- create_filtered_nested_list_by_class(feature_df = clin_data,
                                                               filter_value = "Categorical",
                                                               class_column = "Variable Class",
                                                               internal_column = "FeatureMatrixLabelTSV",
                                                               display_column = "FriendlyLabel",
                                                               filter_column = "VariableType")
      
      var_choices_feat <- create_filtered_nested_list_by_class(feature_df = ioresponse_data$feature_df %>% dplyr::filter(`Variable Class` != "NA"),
                                                               filter_value = "Numeric",
                                                               class_column = "Variable Class",
                                                               internal_column = "FeatureMatrixLabelTSV",
                                                               display_column = "FriendlyLabel",
                                                               filter_column = "VariableType")
      var_choices <- c(var_choices_clin, var_choices_feat)
      
      selectInput(
        ns("var1_surv"),
        "Variable",
        var_choices,
        selected = "IMPRES"
      )
    })
    
    datasets <- reactive({
      switch(
       input$timevar,
       "OS_time" = input$datasets,
       "PFI_time_1"= input$datasets[input$datasets %in% datasets_PFI]
      )
    })
    
    observeEvent(all_survival(),{
      
      if(length(all_survival())>0 & length(datasets()) != length(all_survival())){
        var_label <- convert_value_between_columns(
          input$var1_surv,
          ioresponse_data$feature_df,
          from_column = "FeatureMatrixLabelTSV",
          to_column = "FriendlyLabel"
        )
        output$notification <- renderUI({
          missing_datasets <- paste0(intersect(datasets(), names(all_survival())), collapse = ", ")
          helpText(
            paste("Annotation and/or more than one level for ", var_label, "is available for", missing_datasets, "."))
        })
      }
      if(length(datasets()) == length(all_survival()) | length(all_survival()) == 0){
        output$notification <- renderUI({
        })
      }
    })

    feature_df <- reactive({
      shiny::validate(need(!is.null(input$datasets), "Select at least one dataset."))
      req(input$var1_surv)
      
      ioresponse_data$fmx_df %>% 
            filter(Dataset %in% datasets() & treatment_when_collected == "Pre") %>%
            select(Sample_ID, Dataset, treatment_when_collected, OS, OS_time, PFI_1, PFI_time_1, input$var1_surv)
    })
    
    all_survival <- reactive({
        req(input$var1_surv, !is.null(feature_df()), cancelOutput = T)
        sample_groups <- ioresponse_data$feature_df %>% 
          dplyr::filter(VariableType == "Categorical") %>% 
          dplyr::select(FeatureMatrixLabelTSV, FriendlyLabel) %>% as.vector()
   
        df <- purrr::map(.x = datasets(), df = feature_df(), .f= function(dataset, df){
            dataset_df <- df %>% 
                dplyr::filter(Dataset == dataset)
           
            if(!all(is.na(dataset_df[[input$var1_surv]])) & dplyr::n_distinct(dataset_df[[input$var1_surv]])>1){
              
              surv_df <- build_survival_df(
                df = dataset_df,
                group_column = input$var1_surv,
                group_options = sample_groups$FeatureMatrixLabelTSV,
                time_column = input$timevar,
                k = input$divk,
                div_range = input$div_range
              )
              
              if(input$var1_surv %in% sample_groups$FeatureMatrixLabelTSV){#adding the friendly labels
               
                surv_df <- merge(surv_df, ioresponse_data$sample_group_df %>%
                                   dplyr::filter(Category == input$var1_surv) %>%
                                   dplyr::select(FeatureValue, FeatureLabel),
                                 by.x = "variable", by.y = "FeatureValue")
                surv_df$variable <- NULL
                surv_df <- surv_df %>%
                  dplyr::rename(variable = FeatureLabel)
              }
              surv_df
            } 
        })
        
        names(df) <- datasets()
        Filter(Negate(is.null), df)
    })
    
    all_fit <- reactive({ 
      shiny::validate(need(length(all_survival())>0, "Variable not annotated in the selected dataset(s). Select other datasets or check ICI Datasets Overview for more information."))
      purrr::map(all_survival(), function(df) survival::survfit(survival::Surv(time, status) ~ variable, data = df))
    })
    
    all_kmplot <- reactive({ 
        sample_groups <- ioresponse_data$feature_df %>% dplyr::filter(VariableType == "Categorical") %>% dplyr::select(FeatureMatrixLabelTSV)
        
        if (input$var1_surv %in% sample_groups$FeatureMatrixLabelTSV) { 
          group_colors <- (ioresponse_data$sample_group_df %>% 
            dplyr::filter(Category == input$var1_surv))$FeatureHex
          colors_labels <-(ioresponse_data$sample_group_df %>% 
                             dplyr::filter(Category == input$var1_surv))$FeatureLabel
          
          names(group_colors) <- sapply(colors_labels, function(a) paste('variable=',a,sep=''))
            
        } else if(input$div_range == "median") {
          group_colors <- viridisLite::viridis(2)
        }else{
          group_colors <- viridisLite::viridis(input$divk)
        }
        
        create_kmplot(
            fit = all_fit(),
            df = all_survival(),
            confint = input$confint,
            risktable = input$risktable,
            title = names(all_survival()),
            group_colors = group_colors,
            facet = TRUE)
    })
    
    #the KM Plots are stored as a list, so a few adjustments are necessary to plot everything
    observe({
        output$plots <- renderUI({
          req(input$var1_surv)

            plot_output_list <-
                lapply(1:length(all_survival()), function(i) {
                         plotname <- names(all_survival())[i]
                         plotOutput(ns(plotname), height = 600)
            })
            do.call(tagList, plot_output_list)
        })
    })

    observe({
        lapply(1:length(datasets()), function(i){
            my_dataset <- names(all_survival())[i]
            output[[my_dataset]] <- renderPlot({
              req(input$var1_surv)
                all_kmplot()[i]
                })
        })
    })
}