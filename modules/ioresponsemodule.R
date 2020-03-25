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
              "Plot clicked group?", 
              FALSE
            )
           
          
          #,
          #checkboxInput(ns("group2"), "Select Second Group?", FALSE)
          #div(class = "form-group shiny-input-container", actionButton(ns("go_button"), tags$b("GO"), width = "100%")) 
        ),
        column(
          width = 3,
          p("Select extra Sample Groups (optional)"),
          uiOutput(ns("group2"))
        )
      ),#optionsBox
      fluidRow(
        width = 12,
        plotBox(
          width = 12,
          plotlyOutput(ns("dist_plots"), height = "700px") %>%
            shinycssloaders::withSpinner()
          ),
        plotBox(
          width = 12,
          DT::dataTableOutput(ns("stats1"))
        ),
        conditionalPanel(
          condition =  "input.see_drilldown",
          ns = ns,
          #fluidRow(
            plotBox(
              width = 12,
              plotlyOutput(ns("drilldown_plot")) %>% 
                shinycssloaders::withSpinner()
            )
          #)
        )
       )#fluidRow
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
      dplyr::filter(`Variable Class` %in% c("Immune Checkpoint Treatment - Response", "Immune Checkpoint Treatment - Study Condition"))
    
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
  
  output$group2 <- renderUI({
    lapply(unlist(datasets_options), function(x){
      
      clin_data <- feature_io_df %>% 
        dplyr::filter(`Variable Class` == paste("Clinical data for", x) | `Variable Class` %in% c("Immune Checkpoint Treatment - Response", "Immune Checkpoint Treatment - Study Condition"))
      
      var_choices <- create_filtered_nested_list_by_class(feature_df = clin_data,
                                                          filter_value = "Categorical",
                                                          class_column = "Variable Class",
                                                          internal_column = "FeatureMatrixLabelTSV",
                                                          display_column = "FriendlyLabel",
                                                          filter_column = "VariableType")
      shinyjs::hidden(
        div(id=ns(paste0('div_',x)),
                          selectInput(
                            ns(paste0("dist", x)),
                            label = paste("Select extra group for", as.character(x)),
                            choices = var_choices, #list(split(clin_data$FeatureMatrixLabelTSV, clin_data$FriendlyLabel), "Drug" = "Drug", "Treatment" = "treatment_when_collected")
                            selected = ""
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
  
  output$dist_plots <- renderPlotly({
    shiny::validate(need(!is.null(input$datasets), "Select at least one dataset."))
    
    ylabel <-  convert_value_between_columns(input_value =input$var1_surv,
                                             df = feature_io_df,
                                             from_column = "FeatureMatrixLabelTSV",
                                             to_column = "FriendlyLabel")
    
    all_plots <- purrr::map(.x = input$datasets, function(dataset){
      
      group1 <- input$groupvar
      group2 <- paste0("dist", dataset)
      
      ylabel <-  convert_value_between_columns(input_value =input$var1_surv,
                                               df = feature_io_df,
                                               from_column = "FeatureMatrixLabelTSV",
                                               to_column = "FriendlyLabel")
      
      #Filtering only samples collected pre treatment
      if(input$groupvar != "treatment_when_collected" & input[[group2]] != "treatment_when_collected"){
        dataset_data <- fmx_io %>% 
          dplyr::filter(treatment_when_collected == "Pre")
      }else{
        dataset_data <- fmx_io
      }
      
      
     if(input$groupvar == input[[group2]]){
       
       create_violinplot_onegroup(plot_function(), dataset_data, dataset, input$var1_surv, group1, ylabel)
       
        # dataset_data %>% 
        #   filter_dataset(., 
        #                  dataset, 
        #                  input$var1_surv, 
        #                  group1,
        #                  group2) %>% 
          # `if`(
          #   group1 == "Progression",
          #   get_responder_annot(dataset_data),
          #   .
          # ) %>%
          # `if`(
          #   input$groupvar != input[[group2]],
          #   merge_twogroup(., dataset, 
          #                  input$var1_surv, 
          #                  group1,
          #                  group2,
          #                  ylabel),
          #   .
          # ) %>%
          # plot_function()(., 
          #                   x_col = "Comb_feat", #as.character(group1),
          #                   y_col =  input$var1_surv, 
          #                   xlab = dataset_data[[group1]],
          #                   ylab = ylabel,
          #                   custom_data = as.character(dataset),
          #                   fill_colors = c("#0000FF", "#00FF00", "#FF00FF", "#FF0000"),
          #                   showlegend = F)  %>%
          # add_annotations(
          #   text = dataset,
          #   x = 0.5,
          #   y = 1.1,
          #   yref = "paper",
          #   xref = "paper",
          #   xanchor = "center",
          #   yanchor = "top",
          #   showarrow = FALSE,
          #   font = list(size = 15)
          # ) %>% 
          # `if`(
          #   input$groupvar != input[[group2]],
          #   add_lines_pergroup(., 
          #                  group1,
          #                  group2),
          #   .
          # ) 
            
        # plot_function()(., 
        #                dataset,
        #                input$var1_surv,
        #                input$groupvar,
        #                ylabel)
        
      }else{ #when two grouping levels are selected
        create_violinplot_twogroup(plot_function(),
                                   dataset_data,
                                   dataset,
                                   input$var1_surv,
                                   input$groupvar,
                                   input[[group2]],
                                   ylabel)
      }
     
    })
    
    s <- plotly::subplot(all_plots, shareX = TRUE, shareY = TRUE, nrows = 1, margin = c(0.01, 0.01, 0.01,0.01))
    
    s$x$source <- "distPlots"
    s
  })
  
  
  output$stats1 <- DT::renderDataTable({
    
    purrr::map_dfr(.x = input$datasets, 
                   df = fmx_io, 
                   group_to_split = input$groupvar, 
                   sel_feature = input$var1_surv,
                   .f = get_t_test) 
  })
  
  output$drilldown_plot <- renderPlotly({
    
    eventdata <- event_data("plotly_click", source = "distPlots")
    validate(need(!is.null(eventdata), "Click plot above"))
    clicked_group <- eventdata$x[[1]]
    clicked_dataset <- eventdata$customdata[[1]]
   
    group2 <- paste0("dist", clicked_dataset)
    
    if(input$groupvar != "treatment_when_collected" & input[[group2]] != "treatment_when_collected"){
      dataset_data <- fmx_io %>% 
        dplyr::filter(treatment_when_collected == "Pre")
    }else{
      dataset_data <- fmx_io
    }
    
    if(input$groupvar == input[[group2]]){
      dataset_data <- filter_dataset(dataset_data, clicked_dataset,
                                     input$var1_surv,
                                     input$groupvar)
      
      if(input$groupvar == "Progression"){
        dataset_data <- get_responder_annot(dataset_data) 
        selected_feature <- "Responder"
      }else{
        selected_feature <- as.character(input$groupvar)
      }
      
      dataset_data <- dataset_data %>% 
        dplyr::filter(.[[selected_feature]] == clicked_group)
        
    }else{
      
      dataset_data <- filter_dataset(dataset_data, clicked_dataset,
                                     input$var1_surv,
                                     input$groupvar,
                                     input[[group2]])
      
      
      if(input$groupvar == "Progression"){
        dataset_data <- get_responder_annot(dataset_data) 
        selected_feature <- "Responder"
      }else{
        selected_feature <- input$groupvar
      }
      
      dataset_data$Comb_feat <- paste(dataset_data[[selected_feature]], "&",
                                      convert_value_between_columns(input_value =input[[group2]],
                                                                    df = feature_io_df,
                                                                    from_column = "FeatureMatrixLabelTSV",
                                                                    to_column = "FriendlyLabel"),
                                      ":", dataset_data[[input[[group2]]]], sep = " " )
      selected_feature <- "Comb_feat"
      
      dataset_data <- dataset_data %>% 
        dplyr::filter(Comb_feat == clicked_group)
    }
    
    dataset_data %>% 
      create_histogram(
        x_col = input$var1_surv,
        title = paste(clicked_dataset, clicked_group, sep = ", "), 
        x_lab = convert_value_between_columns(input_value =input$var1_surv,
                                             df = feature_io_df,
                                             from_column = "FeatureMatrixLabelTSV",
                                             to_column = "FriendlyLabel")
      )
  })
}  
