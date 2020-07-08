ioresponsemultivariate_UI <- function(id){
    
    ns <- NS(id)
    
    tagList(
        titleBox("iAtlas Explorer —  Cox Proportional Hazard Ratio to Immune Checkpoint Inhibitors"),
        textBox(
            width = 12,
            p("Create Cox Proportional Hazard Regression Models and visualize Hazard Ratio in a heatmap and a forest plot.")
        ),
        
        sectionBox(
            title = "Cox Proportional Hazard Ratio",

            messageBox(
                width = 24,
                p("Analyze survival data with Cox proportional hazard models. Select the datasets of interest, variables, and outcome in terms of either overall survival (OS) or progression free interval (PFI) 
                  endpoints." ),
                p("Then, select whether the models should be univariable or multivariable. The univariable analysis will compute the Cox proportional hazard ratio (HR) for each combination of selected variable and dataset. The multivariable analysis will compute the HR considering all the selected features 
                  as predictors of time to survival outcome."),
                p("The results are summarized in a heatmap with the log10 of Hazard Ratio. For the univariable analysis, when more than one variable is selected, a Benjamini-Hochberg FDR correction is made, and variables with a p-value smaller than 0.05 and BH pvalue smaller than 0.05 are indicated in the heatmap.
In addition, forest plot with the log10 of the Cox Proportional Hazard Ratio with 95th confidence intervals for each variable is displayed.")
            ),
            
            optionsBox(
                width = 12,
                column(
                  width = 3,
                  checkboxGroupInput(ns("datasets_mult"), "Select Datasets", choices = datasets_options,
                                     selected =  c("Gide 2019", "Hugo 2016", "Riaz 2017", "Van Allen 2015"))
                ),
                column(
                  width = 4,
                  selectInput(
                    ns("timevar"),
                    "Survival Endpoint",
                    c("Overall Survival" = "OS_time",
                      "Progression Free Interval" = "PFI_time_1"),
                    selected = "OS_time"
                  ),
                  selectInput(
                    ns("analysisvar"),
                    "Select Type of Analysis",
                    c("Univariable Cox Proportional" = "uni_coxph",
                      "Multivariable Cox Proportional" = "mult_coxph"
                    ),
                    selected = "uni_coxph"
                  ),
                  actionLink(ns("method_link"), "Click to view method description for each type.")
                ),
                column(
                  width = 5,
                  uiOutput(ns("heatmap_op"))
                )

                ),
            conditionalPanel(condition = paste0("input['", ns("timevar"), "'] == 'PFI_time_1'"),
                             helpText("There is no PFI annotation for Hugo 2016, Riaz 2017, and IMVigor210.")),
            plotBox(
              width = 12,
              plotlyOutput(ns("mult_heatmap"), width = "100%", height = "600px")%>%
                shinycssloaders::withSpinner()
            ),
            plotBox(
                width = 12,
                plotlyOutput(ns("mult_forest"), width = "100%", height = "700px")%>%
                    shinycssloaders::withSpinner()
            )
        ),
        sectionBox(
          title = "Summary Table",
          messageBox(
            width = 24,
            p("The table shows the computed data used to create the visualizations above.")
            ),
          plotBox(
            width = 12,
            DT::dataTableOutput(ns("stats_summary"))
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
        
      var_choices_feat <- create_filtered_nested_list_by_class(feature_df = ioresponse_data$feature_df %>% dplyr::filter(`Variable Class` != "NA"),
                                                            filter_value = "Numeric",
                                                            class_column = "Variable Class",
                                                            internal_column = "FeatureMatrixLabelTSV",
                                                            display_column = "FriendlyLabel",
                                                            filter_column = "VariableType")
      var_choices <- c(var_choices_clin, var_choices_feat)
      
        selectizeInput(
            ns("var2_cox"),
            "Select or Search for variables",
            var_choices,
            selected = c("IMPRES", "Vincent_IPRES_NonResponder"),
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
    
    mult_coxph <- reactive({
      switch(
        input$analysisvar,
        "uni_coxph" = FALSE,
        "mult_coxph" = TRUE
      )
    })
    
    status_column <- reactive({
      switch(
        input$timevar,
        "OS_time" = "OS",
        "PFI_time_1"= "PFI_1"
      )
    })

    feature_df_mult <- reactive({
        
      req(input$datasets_mult, input$var2_cox)
        
      ioresponse_data$fmx_df %>% 
        filter(Dataset %in% datasets()) %>% 
        `if`(
          !("treatment_when_collected" %in% input$var2_cox),
          dplyr::filter(., treatment_when_collected == "Pre"),
          .
        ) %>% 
        dplyr::select(Sample_ID, Dataset, input$timevar, status_column(), treatment_when_collected, dplyr::one_of(input$var2_cox))
    })
    
    dataset_ft <- reactive({
      req(input$datasets_mult, input$var2_cox)
      #creates a df with the dataset x feature combinations that are available
      get_feature_by_dataset(
        datasets = datasets(), 
        features = input$var2_cox, 
        feature_df = ioresponse_data$feature_df, 
        group_df = ioresponse_data$sample_group_df, 
        fmx_df = feature_df_mult()
      )
    })
    
    coxph_df <- reactive({
        req(input$datasets_mult, input$var2_cox)
        build_coxph_df(datasets = datasets(),
                       data = feature_df_mult(), 
                       feature = input$var2_cox,
                       time = input$timevar,
                       status = status_column(),
                       ft_labels = dataset_ft(),
                       multivariate = mult_coxph())
    })
    
    output$mult_forest <- renderPlotly({
      shiny::validate(need(!is.null(input$datasets_mult), "Select at least one dataset."))
     
      all_forests <- purrr::map(.x = unique(coxph_df()$dataset), 
                                .f = build_forestplot_dataset, 
                                coxph_df = coxph_df(),
                                selected_features = input$var2_cox,
                                xname = "log10(Hazard Ratio) + 95% CI") 

      if(length(input$var2_cox) == 1){
        plotly::subplot(all_forests, nrows = dplyr::n_distinct(coxph_df()$dataset), shareX = TRUE, titleX = TRUE, titleY= TRUE, margin = 0.01) 
      }else{
        npannel <- ((dplyr::n_distinct(coxph_df()$dataset)+1)%/%2)
        plotly::subplot(all_forests, nrows = npannel, titleX = TRUE, titleY = TRUE, margin = 0.09)        
      }
    })
    
    output$mult_heatmap <- renderPlotly({
      shiny::validate(need(!is.null(input$datasets_mult), "Select at least one dataset."))

        heatmap_df <-  build_heatmap_df(coxph_df())

        p <- create_heatmap(heatmap_df, "heatmap", scale_colors = T, legend_title = "log10(Hazard Ratio)")
        
        if(mult_coxph() == FALSE & length(input$var2_cox)>1){
          p <- add_BH_annotation(coxph_df(), p)
        }
       p
    })
    
    output$stats_summary <- DT::renderDataTable(
      
      coxph_df() %>% 
        dplyr::select(dataset, Feature = ft_label, Variable = group_label, `log10(HR)` = logHR, loglower, logupper, pvalue, '-log10(p.value)' = logpvalue) %>%
        dplyr::mutate_if(is.numeric, formatC, digits = 3) %>%
        dplyr::arrange(dataset)
    )
    
    observeEvent(input$method_link,{
      showModal(modalDialog(
        title = "Method",
        includeMarkdown("data/markdown/cox_regression.markdown"),
        easyClose = TRUE,
        footer = NULL
      ))
    })
}