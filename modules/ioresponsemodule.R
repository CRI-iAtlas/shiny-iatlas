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
        width=12,
        shinyjs::useShinyjs(),
        column(
          width = 3,
          checkboxGroupInput(ns("datasets"), "Select Datasets", choices = datasets_options,
                             selected = NULL)#c("Gide 2019", "Hugo 2016"))
         ),
        column(
          width = 3,
          uiOutput(ns("feature_op")),
          
          selectInput(
            ns("plot_type"),
            "Select Plot Type",
            choices = c("Violin", "Box")
            ),
            
            selectInput(
              ns("scale_method"), 
              "Select variable scaling",
              choices = c(
                "None", 
                "Log2", 
                "Log2 + 1",
                "Log10",
                "Log10 + 1"
              ),
              selected = "None"
            )
          ),
          column(
            width = 3,
            uiOutput(ns("group1")),
            checkboxInput(
              ns("see_drilldown"), 
              "Display histogram of distribution by clicking on a violin", 
              FALSE
            )
        ),
        column(
          width = 3,
          p("Select extra Sample Groups (optional)"),
          uiOutput(ns("group2"))
        )
      ),#optionsBox
      plotBox(
        width = 12,
        plotlyOutput(ns("dist_plots"), height = "700px") %>%
          shinycssloaders::withSpinner()
        ),
      fluidRow(
        optionsBox(
          width = 3,
          conditionalPanel(
            condition = paste0("input['", ns("groupvar"), "'] == 'treatment_when_collected'"),
            radioButtons(ns("paired"), "Sample type", choices = c("Independent", "Paired"), inline = TRUE, selected = "Paired")),
          conditionalPanel(
            condition = paste0("input['", ns("groupvar"), "'] != 'treatment_when_collected'"),
            radioButtons(ns("paired"), "Sample type", choices = ("Independent"), inline = TRUE, selected = "Independent")),
          radioButtons(ns("stattest"), "Test type", choices = c("t-test", "Wilcox"), inline = TRUE, selected = "t-test")
        ),
        plotBox(
          width = 9,
          DT::dataTableOutput(ns("stats1"))
        )
      ),
      conditionalPanel(
        condition =  "input.see_drilldown",
        ns = ns,
          plotBox(
            width = 12,
            plotlyOutput(ns("drilldown_plot")) %>% 
              shinycssloaders::withSpinner()
          )
      )
     )#, #sectionBox
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
  
  output$group1 <- renderUI({
    
    clin_data <- feature_io_df %>% 
      dplyr::filter(`Variable Class` %in% c("Immune Checkpoint Treatment - Response", 
                                            "Immune Checkpoint Treatment - Study Condition"))
    
    var_choices <- create_filtered_nested_list_by_class(feature_df = clin_data,
                                                        filter_value = "Categorical",
                                                        class_column = "Variable Class",
                                                        internal_column = "FeatureMatrixLabelTSV",
                                                        display_column = "FriendlyLabel",
                                                        filter_column = "VariableType")
    selectInput(
      ns("groupvar"),
      "Select Sample Group",
      var_choices,
      selected = "Progression"
    )
  })
  
  output$group2 <- renderUI({ #Second level group option include dataset-specific classes
    lapply(unlist(datasets_options), function(x){
      
      clin_data <- feature_io_df %>% 
        dplyr::filter(`Variable Class` == paste("Clinical data for", x) | `Variable Class` %in% c("Immune Checkpoint Treatment - Response", "Immune Checkpoint Treatment - Study Condition"))
      
      var_choices <- create_filtered_nested_list_by_class(feature_df = clin_data,
                                                          filter_value = "Categorical",
                                                          class_column = "Variable Class",
                                                          internal_column = "FeatureMatrixLabelTSV",
                                                          display_column = "FriendlyLabel",
                                                          filter_column = "VariableType")
      var_choices[["Default"]] <- c("None" = "None")
      
      shinyjs::hidden(
        div(id=ns(paste0('div_',x)),
                          selectInput(
                            ns(paste0("dist", x)),
                            label = paste("Select extra group for", as.character(x)),
                            choices = var_choices,
                            selected = "None"
                          )
      ))})
  })
  
  # Show all divs that are selected, hide all divs that are not selected.
  observeEvent(input$datasets, #ignoreNULL = FALSE, ignoreInit = FALSE,
               {
                 to_hide = setdiff(unlist(datasets_options),input$datasets)
                 for(x in to_hide)
                 {
                   shinyjs::hide(paste0("div_", x))
                 }
                 to_show = input$datasets
                 for(x in to_show)
                 {
                  shinyjs::show(paste0('div_',x))
                 }
               })
  
  
  plot_function <- reactive({
    switch(
      input$plot_type,
      "Violin" = create_violinplot,
      "Box" = create_boxplot
    )
  })
  
  varible_display_name <- reactive({
    
    convert_value_between_columns(input_value =input$var1_surv,
                                  df = feature_io_df,
                                  from_column = "FeatureMatrixLabelTSV",
                                  to_column = "FriendlyLabel")
  })
  
  varible_plot_label <- reactive({
    switch(
      input$scale_method,
      "None" = varible_display_name(),
      
      "Log2" = stringr::str_c(
        "Log2( ", 
        varible_display_name(),
        " )"),
      
      "Log2 + 1" = stringr::str_c(
        "Log2( ", 
        varible_display_name(),
        " + 1 )"),
      
      "Log10" = stringr::str_c(
        "Log10( ", 
        varible_display_name(), 
        " )"),
      
      "Log10 + 1" = stringr::str_c(
        "Log10( ", 
        varible_display_name(), 
        " + 1 )")
    )
  })
  
  df_selected <- reactive({
    
    all_groups <- lapply(unlist(datasets_options), function(x) input[[paste0("dist", x)]]) %>% 
      unlist() %>% 
      unique()
    
    group_df <- fmx_io %>% 
      dplyr::select(Sample_ID, Patient_ID, Dataset, treatment_when_collected, input$groupvar, dplyr::one_of(all_groups)) 
    
    # group_df <- purrr::map_dfr(unlist(datasets_options), df = group_df, function(x, df){
    #   g2 <- paste0("dist", x)
    #   
    #   print(input[[g2]])
    #   print(input$groupvar)
    #   df %>% 
    #     filter(Dataset == x) %>% 
    #     dplyr::mutate(group = dplyr::case_when(
    #       input[[g2]] != "None"  ~ combine_groups(., input$groupvar, input[[g2]]),
    #       input[[g2]] == "None" | input$groupvar == input[[g2]] ~ .[[input$groupvar]]
    #     ))
    # })
    
    
    new_df <- fmx_io %>% 
      dplyr::select(Sample_ID, input$var1_surv) %>%
      tidyr::drop_na() %>% 
      build_distribution_io_df(.,
                               feature = .[[input$var1_surv]],
                               scale_func_choice = input$scale_method)
    
    merge(group_df, new_df, by = "Sample_ID")
  })
  
  output$dist_plots <- renderPlotly({
    shiny::validate(need(!is.null(input$datasets), "Select at least one dataset."))
    
    all_plots <- purrr::map(.x = input$datasets, function(dataset){
      
      group1 <- input$groupvar
      group2 <- paste0("dist", dataset)
      
      
      #Filtering only samples collected pre treatment
      if(input$groupvar != "treatment_when_collected" & input[[group2]] != "treatment_when_collected"){
        dataset_data <- df_selected() %>% 
          dplyr::filter(treatment_when_collected == "Pre")
      }else{
        dataset_data <- df_selected()
      }
    
     if(input[[group2]] == "None" | input$groupvar == input[[group2]]){#only one group selected
       dataset_data %>%
         dplyr::filter(Dataset == dataset) %>% 
         create_plot_onegroup(., 
                              plot_function(), 
                              dataset, 
                              "y", 
                              group1, 
                              varible_plot_label())
        
      }else{ #when two grouping levels are selected
        
        dataset_data %>%
          dplyr::filter(Dataset == dataset) %>%
          create_plot_twogroup(.,
                               plot_function(),
                               dataset,
                               "y",
                               input$groupvar,
                               input[[group2]],
                               varible_plot_label())
      }
    })
    
    s <- plotly::subplot(all_plots, shareX = TRUE, shareY = TRUE, nrows = 1, margin = c(0.01, 0.01, 0.01,0.01))
    
    s$x$source <- "distPlots"
    s
  })
  
  paired_test <- reactive({
    switch(
      input$paired,
      "Paired" = TRUE,
      "Independent" = FALSE
    )
  })
  
  test_function <- reactive({
    switch(
      input$stattest,
      "t-test" = t.test,
      "Wilcox" = wilcox.test
    )
  })
  
  output$stats1 <- DT::renderDataTable({
    req(input$groupvar)
    group_label <- convert_value_between_columns(input_value = input$groupvar,
                                  df = feature_io_df,
                                  from_column = "FeatureMatrixLabelTSV",
                                  to_column = "FriendlyLabel")
    
    purrr::map_dfr(.x = input$datasets, 
                   df = df_selected(), 
                   group_to_split = input$groupvar, 
                   sel_feature = input$var1_surv,
                   paired = paired_test(),
                   test = test_function(),
                   label = group_label,
                   .f = get_t_test) 
  })
  
  output$drilldown_plot <- renderPlotly({
    
    eventdata <- event_data("plotly_click", source = "distPlots")
    validate(need(!is.null(eventdata), "Click plot above"))
    clicked_group <- eventdata$x[[1]]
    clicked_dataset <- eventdata$customdata[[1]]
   
    group2 <- paste0("dist", clicked_dataset)
    
    dataset_data <-  df_selected() %>% 
      dplyr::filter(Dataset == clicked_dataset)
    
    if(input$groupvar != "treatment_when_collected" & input[[group2]] != "treatment_when_collected"){
      dataset_data <- dataset_data %>% 
        dplyr::filter(treatment_when_collected == "Pre")
    }

    if(input$groupvar == "Progression"){
      dataset_data <- get_responder_annot(dataset_data) 
      selected_group <- "Responder"
    }else{
      selected_group <- as.character(input$groupvar)
    }
    
    
    if(input[[group2]] == "None" | input$groupvar == input[[group2]]){
      
      dataset_data <- dataset_data %>% 
        dplyr::filter(.[[selected_group]] == clicked_group)
        
    }else{#two groups selected
      
      dataset_data <- combine_groups(dataset_data, selected_group, input[[group2]]) %>% 
        dplyr::filter(Comb_feat == clicked_group)
     }
    
    dataset_data %>% 
      create_histogram(
        x_col = "y",
        title = paste(clicked_dataset, clicked_group, sep = ", "), 
        x_lab = varible_plot_label()
      )
  })
}  
