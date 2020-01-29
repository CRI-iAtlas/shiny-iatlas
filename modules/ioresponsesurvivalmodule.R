#loading data 
#The path for the feature matrix needs to be informed. 
#In unix bash shell, this can be set by running: export IO_PATH=path/to/files/
#In a R Session, this can be set with the command in the console: Sys.setenv(IO_PATH="path/to/files/")

IO_PATH = Sys.getenv("IO_PATH")

fmx_io <- feather::read_feather(paste(IO_PATH, "fmx_io.feather", sep = ""))
feature_io_df <- feather::read_feather(paste(IO_PATH, "feature_io_df.feather", sep = ""))
dataset_io_df <- feather::read_feather(paste(IO_PATH, "datasets_io_df.feather", sep = ""))

iosurvival_UI <- function(id){
    
    ns <- NS(id)
     
    tagList(
        titleBox("iAtlas Explorer — Clinical Outcomes to Immune Checkpoint Inhibitors"),
        textBox(
            width = 12,
            p("Explore the ‘omics’ data sets on response to checkpoint inhibitors treatments")
        ),
        
        sectionBox(
            title = "Clinical Outcomes",
            
            messageBox(
                width = 24,
                p("This module generates different analysis of response of immune checkpoint inhibitors (ICI) treatment. You can select the datasets of interest and the immuno features for analysis.
                  A Kaplan Meyer plot will be generated for each selected dataset. In addition, it will be generated a forest plot with the log10 of Cox Proportional Hazard ratio with 95% confidence interval for the selected feature for each dataset.")
               
            ),
            
            optionsBox(
                width=3,
                verticalLayout(
                        fluidRow(
                            column(
                                width = 5,
                                checkboxGroupInput(ns("datasets"), "Select Datasets", choices = c("Gide 2019", "Hugo 2016", 
                                                                                                  "IMVigor210", "Prins 2019", "Riaz 2017", "Van Allen 2015"),
                                                   selected = "Gide 2019")
                                #the Auslander dataset does not have annotation for OS  
                            ),
                            column(
                                width = 7,
                                selectizeInput(ns("types"), "Select tumor(s) type(s)", choices = c("All", unique(dataset_io_df$Study)), selected = NULL),
                                selectizeInput(ns("therapy"), "Select therapy(ies) type(s)", choices = c("All", unique(dataset_io_df$Antibody)), selected = NULL),
                                selectizeInput(ns("drugs"), "Select drug(s) of treatment", choices = c("All", unique(dataset_io_df$Drug)), selected = NULL)
                            )
                        ),    
                        uiOutput(ns("survplot_op")),
                        checkboxInput(ns("confint"), "Confidence Intervals", value = F),
                        checkboxInput(ns("risktable"), "Risk Table", value = T),
                   
                        selectInput(
                            ns("timevar"),
                            "Survival Endpoint",
                            c("Overall Survival" = "OS_time"),
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
                                         )),
                        div(class = "form-group shiny-input-container", actionButton(ns("go_button"), tags$b("GO"), width = "100%"))
                )
                
            ),#optionsBox
            column(
                width = 9,
                plotBox(
                    width = 12,
                    plotlyOutput(ns("forest"), height = "450px") %>% 
                        shinycssloaders::withSpinner()
                ),
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
                       session, 
                       group_display_choice,
                       group_internal_choice,
                       study_subset_choice,
                       sample_group_df,
                       subset_df,
                       plot_colors){
    
    ns <- session$ns
    
    output$survplot_op <- renderUI({
      
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
    
    #Update dataset selection based on choice of study design
    
    dataset_select <- reactive({
      
      if(input$types == "All"){
        subset_dataset <- dataset_io_df %>%
          filter(Dataset != "Auslander 2018") %>% 
          select(Dataset, Study, Antibody, Drug) %>% unique() 
      }else{
        subset_dataset <- dataset_io_df %>%
          filter(Dataset != "Auslander 2018" & Study == input$types) %>%
          select(Dataset, Study, Antibody, Drug) %>% unique() 
      }
      
      if(input$therapy != "All"){
        subset_dataset <- subset_dataset %>% 
          filter(Antibody == input$therapy)
      }else{
        subset_dataset
      }
      
      if(input$drugs != "All"){
        subset_dataset <- subset_dataset %>% 
          filter(Drug == input$drugs)
      }else{
        subset_dataset
      }
      
      return(subset_dataset)
    })
    
    observeEvent(dataset_select(),{
      #browser()
      updateCheckboxGroupInput(
        session,
        "datasets",
        choices = c("Gide 2019", "Hugo 2016", "IMVigor210", "Prins 2019", "Riaz 2017", "Van Allen 2015"),
        selected = dataset_select() %>% dplyr::select(Dataset) %>% unique() %>% .[[1]]
      )
      #updateSelectInput(session = session, inputId = "therapy", choices = c("All", dataset_select() %>% dplyr::select(Antibody) %>%.[[1]]),  selected = "All")
      #updateSelectInput(session, "drugs", choices = c("All", dataset_select() %>% dplyr::select(Drug) %>%.[[1]]))
    })
    
    observeEvent(input$types,{
     # browser()
#      updateSelectInput(session, "therapy", choices = c("All", dataset_select() %>% dplyr::select(Antibody) %>%.[[1]]), selected = "All")
      updateSelectInput(session, "therapy", selected = "All")
    })
    
    observeEvent(input$therapy,{
      #updateSelectInput(session, "types", choices = c("All", dataset_select() %>% dplyr::select(Study) %>%.[[1]]), selected = current_type())
      updateSelectInput(session, "drugs", choices = c("All", dataset_select() %>% dplyr::select(Drug) %>%.[[1]]), selected = "All")
    })
    
                 
    feature_df <- eventReactive(input$go_button,{
      
        fmx_io %>% 
            filter(Dataset %in% input$datasets & treatment_when_collected == "Pre") %>%
            select(Sample_ID, Dataset, treatment_when_collected, OS, OS_time, input$var1_surv)
    })
    
    all_survival <- eventReactive(input$go_button,{
        
        req(!is.null(feature_df()), cancelOutput = T)
        sample_groups <- feature_io_df %>% filter(VariableType == "Categorical") %>% select(FeatureMatrixLabelTSV) %>% as.vector()
        n_groups <- dplyr::n_distinct(sample_groups)
        
        #validate(need(input$var1_surv, "Waiting for input."))
        
        
        if(input$div_range == "median"){
          breaks <- 2
          break_median <- TRUE
        }else{
          breaks <- input$divk
          break_median <- FALSE
        }

        purrr::map(.x = input$datasets, df = feature_df(), .f= function(dataset, df){
            dataset_df <- df %>% 
                dplyr::filter(Dataset == dataset)
            
            build_survival_df(
                df = dataset_df,
                group_column = input$var1_surv,
                group_options = sample_groups$FeatureMatrixLabelTSV,
                time_column = "OS_time",
                k = breaks,
                by.median = break_median
            )
        })
    })
    
    all_fit <- eventReactive(input$go_button,{
        purrr::map(all_survival(), function(df) survival::survfit(survival::Surv(time, status) ~ variable, data = df))
    })
    
    all_kmplot <- eventReactive(input$go_button,{
        
        sample_groups <- feature_io_df %>% filter(VariableType == "Categorical") %>% select(FeatureMatrixLabelTSV)
        
        if (input$var1_surv %in% sample_groups$FeatureMatrixLabelTSV) { 
          group_colors <- viridisLite::viridis(dplyr::n_distinct(fmx_io[[input$var1_surv]]))
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
            title = input$datasets,
            group_colors = group_colors,
            facet = TRUE)
    })
    
    #the KM Plots are stored as a list, so a few adjustments are necessary to plot everything
    observe({
        output$plots <- renderUI({

            plot_output_list <-
                lapply(1:length(input$datasets), function(i) {
                         plotname <- input$datasets[i]
                         plotOutput(ns(plotname), height = 400, width = 750)
            })
            do.call(tagList, plot_output_list)
        })
    })

    observe({
        lapply(1:length(input$datasets), function(i){
            my_dataset <- input$datasets[i]
            output[[my_dataset]] <- renderPlot({
                all_kmplot()[i]
                })
        })
    })

   
#forest plot     
    output$forest <- renderPlotly({
      req(!is.null(feature_df()), input$var1_surv %in% colnames(feature_df()))

        all_hr <- purrr::map(.x = input$datasets, data = feature_df(), variable = input$var1_surv, .f= function(dataset, data, variable){
            data_cox <- data %>%
                filter(Dataset == dataset)
            
            survival::coxph(as.formula(paste("survival::Surv(OS_time, OS) ~ ", variable)), data_cox)
        })

        names(all_hr) <- input$datasets
      
        create_ph_df <- function(coxphList){
            
            coef_stats <- as.data.frame(summary(coxphList)$conf.int)
            coef_stats$feature <- row.names(coef_stats)
            coef_stats
        }
        
        cox_coef <- purrr::map(all_hr, create_ph_df)
        log_meta_stats <- data.table::rbindlist(cox_coef, idcol = TRUE)
        
        log_meta_stats <- log_meta_stats %>% 
            mutate(logHR = log10(`exp(coef)`),
                   logupper = log10(`upper .95`),
                   loglower = log10(`lower .95`))
        
        print(log_meta_stats)
        
        title <- convert_value_between_columns(input_value = input$var1_surv,
                                               df = feature_io_df,
                                               from_column = "FeatureMatrixLabelTSV",
                                               to_column = "FriendlyLabel")
        
       title <- paste(title, " - Forest Plot")
        
        if(nrow(log_meta_stats)== base::length(input$datasets)){
            create_forestplot(log_meta_stats,
                              x=log_meta_stats$logHR,
                              y=log_meta_stats$.id,
                              xmin=log_meta_stats$loglower,
                              xmax=log_meta_stats$logupper,
                              xintercept = 0,
                              xlab = 'Hazard Ratio (log10)',
                              ylab = "Reference",
                              title = title)
        }else{
          #when feature is a categorical feature, such as response
            create_forestplot(log_meta_stats,
                              x=log_meta_stats$logHR,
                              y=log_meta_stats$feature,
                              xmin=log_meta_stats$loglower,
                              xmax=log_meta_stats$logupper,
                              xintercept = 0,
                              xlab = 'Hazard Ratio (log10)',
                              #ylab = "Reference",
                              title = title,
                              facet = ".id")
        }
    })
 
}