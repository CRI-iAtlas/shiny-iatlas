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
          width = 4,
          checkboxGroupInput(ns("datasets"), "Select Datasets", choices = datasets_options,
                             selected = NULL)#c("Gide 2019", "Hugo 2016"))
         ),
        column(
          width = 4,
          uiOutput(ns("feature_op")),
          uiOutput(ns("group1"))
          #,
          #checkboxInput(ns("group2"), "Select Second Group?", FALSE)
          #div(class = "form-group shiny-input-container", actionButton(ns("go_button"), tags$b("GO"), width = "100%")) 
        ),
        column(
          width = 4,
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
      dplyr::filter(`Variable Class` == "Immune Checkpoint Treatment")
    
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
        dplyr::filter(`Variable Class` == paste("Clinical data for", x) | `Variable Class` == "Immune Checkpoint Treatment")
      
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
                            selected = "Progression"
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
  
  output$dist_plots <- renderPlotly({
    shiny::validate(need(!is.null(input$datasets), "Select at least one dataset."))
    
    ylabel <-  convert_value_between_columns(input_value =input$var1_surv,
                                             df = feature_io_df,
                                             from_column = "FeatureMatrixLabelTSV",
                                             to_column = "FriendlyLabel")
    
    all_plots <- purrr::map(.x = input$datasets, function(dataset){
      
      group1 <- input$groupvar
      group2 <- paste0("dist", dataset)
      
      if(input$groupvar == input[[group2]]){
        dataset_data <- filter_dataset(fmx_io, 
                                       dataset, 
                                       input$var1_surv, 
                                       input$groupvar)
        
        if(input$groupvar == "Progression"){
          dataset_data <- get_responder_annot(dataset_data)
          group1 <- "Responder"
          }
        
        create_violinplot(dataset_data, 
                          x_col = as.character(group1),
                          y_col = input$var1_surv, 
                          xlab = dataset_data[[group1]],
                          ylab = ylabel,
                          fill_colors = c("#0000FF", "#00FF00", "#FF00FF", "#FF0000"),
                          showlegend = F)  %>%
          add_annotations(
            text = dataset,
            x = 0.5,
            y = 1.1,
            yref = "paper",
            xref = "paper",
            xanchor = "center",
            yanchor = "top",
            showarrow = FALSE,
            font = list(size = 15)
          )
      }else{ #when two grouping levels are selected
        dataset_data <- filter_dataset(fmx_io, dataset,
                                       input$var1_surv,
                                       input$groupvar,
                                       input[[group2]])
        
        if(input$groupvar == "Progression"){
          dataset_data <- get_responder_annot(dataset_data) 
          group1 <- "Responder"
        }
          
        dataset_data$Comb_feat <- paste(dataset_data[[group1]], "&",
                                        convert_value_between_columns(input_value =input[[group2]],
                                                                      df = feature_io_df,
                                                                      from_column = "FeatureMatrixLabelTSV",
                                                                      to_column = "FriendlyLabel"),
                                        ":", dataset_data[[input[[group2]]]], sep = " " )
        
        #get number of groups to draw lines
        samples <- (dataset_data %>% group_by(dataset_data[[group1]], dataset_data[[input[[group2]]]]) %>% 
                      summarise(samples = n()))
        colnames(samples) <- c("var1", "var2", "samples")
        
        ylabel <-  convert_value_between_columns(input_value =input$var1_surv,
                                                 df = feature_io_df,
                                                 from_column = "FeatureMatrixLabelTSV",
                                                 to_column = "FriendlyLabel")
        
        create_violinplot(dataset_data,
                          x_col = "Comb_feat",
                          y_col = input$var1_surv,
                          xlab = (dataset_data[[input[[group2]]]]),
                          ylab = ylabel,
                          fill_colors = c("#0000FF", "#00FF00", "#FF00FF", "#FF0000"),
                          showlegend = F) %>%
          add_annotations(
            text = dataset,
            x = 0.5,
            y = 1.1,
            yref = "paper",
            xref = "paper",
            xanchor = "center",
            yanchor = "top",
            showarrow = FALSE,
            font = list(size = 15)) %>% 
          layout(
            xaxis = list(tickangle = 80),
            shapes = lazyeval::lazy_eval(get_lines_pos(samples, -0.38)),
            margin = list(b = 10)
            #annotations = lazyeval::lazy_eval(get_text_pos(dataset_data, input$groupvar, -0.50))
          )
      }
     
    })
    
    plotly::subplot(all_plots, shareX = TRUE, shareY = TRUE, nrows = 1, margin = c(0.01, 0.01, 0.01,0.01))
  })
  
  
  output$stats1 <- DT::renderDataTable({
    
    purrr::map_dfr(.x = input$datasets, 
                   df = fmx_io, 
                   group_to_split = input$groupvar, 
                   sel_feature = input$var1_surv,
                   .f = get_t_test) 
  })
}  
