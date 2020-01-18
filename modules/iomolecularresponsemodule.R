#loading data (to be changed to a DB)

fmx_io <- feather::read_feather(paste(IO_DATA, "fmx_io.feather", sep = ""))
feature_io_df <- feather::read_feather(paste(IO_DATA, "feature_io_df.feather", sep = ""))

ioresponse_UI <- function(id){
    
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
                                                   selected = c("Gide 2019", "Hugo 2016", 
                                                                "IMVigor210", "Prins 2019", "Riaz 2017", "Van Allen 2015"))
                                #the Auslander dataset does not have annotation for OS  
                            ),
                            column(
                                width = 7,
                                selectizeInput(ns("types"), "Select tumor(s) type(s)", choices = unique(sample$Tissue)),
                                selectizeInput(ns("therapy"), "Select therapy(ies) type(s)", choices = unique(sample$Antibody)),
                                selectizeInput(ns("drugs"), "Select drug(s) of treatment", choices = unique(sample$Drug))
                            )
                        ),    
                        uiOutput(ns("survplot_op")),
                        #checkboxGroupInput(ns("response"), "Select Response Categories", choices = c("OS_d")),
                        #c("OS_d", "OS_y", "OS_e", "PR", "CR", "SD")
                        checkboxInput(ns("confint"), "Confidence Intervals", value = F),
                        checkboxInput(ns("risktable"), "Risk Table", value = T),
                   
                        selectInput(
                            ns("timevar"),
                            "Survival Endpoint",
                            c("Overall Survival" = "OS_time"),
                            selected = "OS_time"
                        ),
                        
                        sliderInput(
                            ns("divk"),
                            "Value Range Divisions",
                            min = 2,
                            max = 10,
                            value = 2
                        ),
                        # checkboxGroupInput(ns("analyses"), "Select Analyses", choices = c("Groupwise Comparison", "CoxPH")),
                        # br(),
                        div(class = "form-group shiny-input-container", actionButton(ns("go_button"), tags$b("GO"), width = "100%"))
                )
                
            ),#optionsBox
            column(
                width = 9,
                plotBox(
                    width = 12,
                    plotlyOutput(ns("forest")) %>% 
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
    
    output$survplot_op <- renderUI({
        #group_choice <- magrittr::set_names(list(group_internal_choice()), ss_choice())
        var_choices <- feature_io_df$FeatureMatrixLabelTSV[4:72]
        selectInput(
            ns("var1_surv"),
            "Variable",
            var_choices,
            selected = var_choices[1]
        )
    })
    

    feature_df <- reactive({
      
        fmx_io %>% 
            filter(Dataset %in% input$datasets & treatment_when_collected == "Pre") %>%
            select(Sample_ID, Dataset, treatment_when_collected, OS, OS_time, input$var1_surv)
    })
    
    all_survival <- reactive({
        
        req(!is.null(feature_df()), cancelOutput = T)
        sample_groups <- feature_io_df %>% filter(VariableType == "Categorical") %>% select(FeatureMatrixLabelTSV) %>% as.vector()
        n_groups <- dplyr::n_distinct(sample_groups)
        
        validate(
            need(input$var1_surv, "Waiting for input.")
            # need(dplyr::n_distinct(sample_groups) <= 10, 
            #      paste0("Too many sample groups (", n_groups, ") for KM plot; ",
            #             "choose a continuous variable or select different sample groups."))
        )
        
        
        purrr::map(.x = input$datasets, df = feature_df(), .f= function(dataset, df){
            dataset_df <- df %>% 
                dplyr::filter(Dataset == dataset)
            
            build_survival_df(
                df = dataset_df,
                group_column = input$var1_surv,
                group_options = sample_groups$FeatureMatrixLabelTSV,
                time_column = "OS_time",
                k = input$divk
            )
        })
    })
    
    all_fit <- reactive({
        purrr::map(all_survival(), function(df) survival::survfit(survival::Surv(time, status) ~ variable, data = df))
    })
    
    all_kmplot <- reactive({
        
        sample_groups <- feature_io_df %>% filter(VariableType == "Categorical") %>% select(FeatureMatrixLabelTSV)
        
        if (input$var1_surv %in% sample_groups$FeatureMatrixLabelTSV) { 
            group_colors <- viridisLite::viridis(dplyr::n_distinct(fmx_io[[input$var1_surv]]))
        } else {
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
        # meta_stats <- data.frame("Dataset" = names(all_hr),
        #                          "coef"= sapply(all_hr, with, coefficients),
        #                          "exp.coef" = sapply(all_hr,  function(l) summary(l)$coef[2]),
        #                          "SE"= sapply(all_hr,  function(l) summary(l)$coef[3]),
        #                          "upper" = sapply(all_hr,  function(l) summary(l)$conf.int[3]),
        #                          "lower" = sapply(all_hr,  function(l) summary(l)$conf.int[4]))
        # print(meta_stats)
        # log_meta_stats <- meta_stats %>%
        #     mutate(logHR = log10(exp.coef),
        #            logupper = log10(upper),
        #            loglower = log10(lower))
     
        
        if(nrow(log_meta_stats)== length(input$datasets)){
            create_forestplot(log_meta_stats,
                              x=log_meta_stats$logHR,
                              y=log_meta_stats$.id,
                              xmin=log_meta_stats$loglower,
                              xmax=log_meta_stats$logupper,
                              xintercept = 0,
                              xlab = 'Hazard Ratio (log10)',
                              ylab = "Reference",
                              title = input$var1_surv)
        }else{
            create_forestplot(log_meta_stats,
                              x=log_meta_stats$logHR,
                              y=log_meta_stats$feature,
                              xmin=log_meta_stats$loglower,
                              xmax=log_meta_stats$logupper,
                              xintercept = 0,
                              xlab = 'Hazard Ratio (log10)',
                              ylab = "Reference",
                              title = input$var1_surv,
                              facet = ".id")
        }
    })
 
}





# ##Insert the right number of plot output objects into the web page
# observe({
#     output$plots <- renderUI({
#        
#         plot_output_list <- 
#             lapply(1:length(input$datasets), function(i) {
#                      plotname <- input$datasets[i]
#                      plotOutput(ns(plotname), height = 400, width = 750)
#         })
#         do.call(tagList, plot_output_list)
#     })
# })
# 
# # Call renderPlot for each one. Plots are only actually generated when they
# # are visible on the web page.
# observe({
#     for (i in 1:length(input$datasets)) {
#             # Need local so that each item gets its own number. Without it, the value of i in the renderPlot() will be the same across all instances, 
#             # because of when the expression is evaluated.
#             local({
#                 
#                 my_i <- i
#                 plotname <- input$datasets[my_i]
#                 my_var <- input$var1_surv
#                 
#                 req(!is.null(my_var))
#                 survival_data <- sample %>%
#                     filter(Dataset == input$datasets[my_i] & Treatment == "Pre") %>%
#                     select(Sample_ID, Dataset, Treatment, OS_d, OS_e) %>%
#                     dplyr::rename(OS = OS_e, OS_time = OS_d)
#                 
#                 #Var_data
#                 var_data <- panimmune_sigs %>%
#                     select(Sample_ID, my_var)
#                 
#                 outcome <- merge(survival_data, var_data, by = "Sample_ID")
#               
#                 survival_df <- build_survival_df(
#                     df = outcome,
#                     group_column = my_var,
#                     group_options = "Other",
#                     time_column = "OS_time",
#                     k = input$divk
#                 )
#                 
#                 survival_df %>%
#                     dplyr::group_by(variable) %>%
#                     dplyr::summarize(Num1 = sum(status == 1), Num0 = sum(status == 0))
#                 
#                 print(head(survival_df))
#                 fit <- survival::survfit(survival::Surv(time, status) ~ variable, data = survival_df)
#                 
#                 group_colors <- viridisLite::viridis(input$divk)
#                 
#                 output[[plotname]] <- renderPlot({
#                     
#                     create_kmplot(
#                         fit = fit,
#                         df = survival_df,
#                         confint = input$confint,
#                         risktable = input$risktable,
#                         title = plotname,
#                         group_colors = group_colors)
#                 })#renderPlot
#             })#local
#         }#for
#      }) #observe
   