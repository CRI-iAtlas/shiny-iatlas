#loading data 
#The path for the feature matrix needs to be informed. 
#In unix bash shell, this can be set by running: export IO_PATH=path/to/files/
#In a R Session, this can be set with the command in the console: Sys.setenv(IO_PATH="path/to/files/")

IO_PATH = Sys.getenv("IO_PATH")

fmx_io <- feather::read_feather(paste(IO_PATH, "fmx_io.feather", sep = ""))
feature_io_df <- feather::read_feather(paste(IO_PATH, "feature_io_df.feather", sep = ""))
im_expr_io_df <- feather::read_feather(paste(IO_PATH, "im_expr_io.feather", sep = ""))
dataset_io_df <- feather::read_feather(paste(IO_PATH, "datasets_io_df.feather", sep = ""))

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
For a continuous (numeric) variable, the range can be split in the median of the interval, or into equal intervals of the value range. For the latter,  the slider can be used to specify how the range of values of that variable is split. Selecting 2 splits the values by the middle of the range, 
3 splits the range into three even intervals and so on."),
                p("In addition, the selection of datasets, variable and outcome of interest generate a Forest Plot with the log10 of the 
                  Cox Proportional Hazard Ratio with 95th confidence intervals for each selected dataset.")
            ),
            
            optionsBox(
                width=3,
                verticalLayout(
                        fluidRow(
                            column(
                                width = 12,
                                checkboxGroupInput(ns("datasets"), "Select Datasets", choices = datasets_options,
                                                   selected = NULL)
                                #the Auslander dataset does not have annotation for OS  
                            # ),
                            # column(
                            #     width = 7,
                            #     selectizeInput(ns("types"), "Select tumor(s) type(s)", choices = c("All", unique(dataset_io_df$Study)), selected = NULL),
                            #     selectizeInput(ns("therapy"), "Select therapy(ies) type(s)", choices = c("All", unique(dataset_io_df$Antibody)), selected = NULL),
                            #     selectizeInput(ns("drugs"), "Select drug(s) of treatment", choices = c("All", unique(dataset_io_df$Drug)), selected = NULL)
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
                                         ))#,
#                        div(class = "form-group shiny-input-container", actionButton(ns("go_button"), tags$b("GO"), width = "100%"))
                )
                
            ),#optionsBox
            column(
                width = 9,
                conditionalPanel(condition = paste0("input['", ns("timevar"), "'] == 'PFI_time_1'"),
                                 helpText("There is no PFI annotation for Hugo 2016, Riaz 2017, and IMVigor210.")),
                plotBox(
                    width = 12,
                    uiOutput(ns("plots")) %>% 
                        shinycssloaders::withSpinner()
                ),
                plotBox(
                    width = 12,
                    plotlyOutput(ns("forest"), height = "450px") %>%
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
    
    datasets <- reactive({
      switch(
       input$timevar,
       "OS_time" = input$datasets,
       "PFI_time_1"= input$datasets[input$datasets %in% c("Gide 2019", "Van Allen 2015", "Prins 2019")]
      )
    })

                 
    feature_df <- reactive({ #eventReactive(input$go_button,{
      shiny::validate(need(!is.null(input$datasets), "Select at least one dataset."))
        fmx_io %>% 
            filter(Dataset %in% datasets() & treatment_when_collected == "Pre") %>%
            select(Sample_ID, Dataset, treatment_when_collected, OS, OS_time, PFI_1, PFI_time_1, input$var1_surv)
    })
    
    all_survival <- reactive({ #eventReactive(input$go_button,{
        
        req(!is.null(feature_df()), cancelOutput = T)
        sample_groups <- feature_io_df %>% filter(VariableType == "Categorical") %>% select(FeatureMatrixLabelTSV) %>% as.vector()
        n_groups <- dplyr::n_distinct(sample_groups)

        purrr::map(.x = datasets(), df = feature_df(), .f= function(dataset, df){
            dataset_df <- df %>% 
                dplyr::filter(Dataset == dataset)
            
            build_survival_df(
              df = dataset_df,
              group_column = input$var1_surv,
              group_options = sample_groups$FeatureMatrixLabelTSV,
              time_column = input$timevar,
              k = input$divk,
              div_range = input$div_range
            )
        })
    })
    
    all_fit <- reactive({ #eventReactive(input$go_button,{
        purrr::map(all_survival(), function(df) survival::survfit(survival::Surv(time, status) ~ variable, data = df))
    })
    
    all_kmplot <- reactive({ # eventReactive(input$go_button,{
        
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
            title = datasets(),
            group_colors = group_colors,
            facet = TRUE)
    })
    
    #the KM Plots are stored as a list, so a few adjustments are necessary to plot everything
    observe({
        output$plots <- renderUI({

            plot_output_list <-
                lapply(1:length(datasets()), function(i) {
                         plotname <- datasets()[i]
                         plotOutput(ns(plotname), height = 400, width = 750)
            })
            do.call(tagList, plot_output_list)
        })
    })

    observe({
        lapply(1:length(datasets()), function(i){
            my_dataset <- datasets()[i]
            output[[my_dataset]] <- renderPlot({
                all_kmplot()[i]
                })
        })
    })

   
#forest plot     
    output$forest <- renderPlotly({
#      shiny::validate(need(!is.null(input$datasets), "Select at least one dataset."))
#      req(!is.null(feature_df()), input$var1_surv %in% colnames(feature_df()))
      shiny::req(!is.null(feature_df()), cancelOutput = T)

        all_hr <- purrr::map(.x = datasets(), data = feature_df(), variable = input$var1_surv, .f= function(dataset, data, variable){
            data_cox <- data %>%
                filter(Dataset == dataset)
            
            survival::coxph(as.formula(paste("survival::Surv(OS_time, OS) ~ ", variable)), data_cox)
        })

        names(all_hr) <- datasets()
      
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
        
        title <- convert_value_between_columns(input_value = input$var1_surv,
                                               df = feature_io_df,
                                               from_column = "FeatureMatrixLabelTSV",
                                               to_column = "FriendlyLabel")
        
       title <- paste(title, " - Forest Plot")
        
        if(nrow(log_meta_stats)== base::length(datasets())){
            create_forestplot(log_meta_stats,
                              x=log_meta_stats$logHR,
                              y=log_meta_stats$.id,
                              xmin=log_meta_stats$loglower,
                              xmax=log_meta_stats$logupper,
                              xintercept = 0,
                              xlab = 'Hazard Ratio (log10)',
                              #ylab = "Reference",
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