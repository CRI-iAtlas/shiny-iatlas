#loading data (to be changed to a DB)

IO_DATA="/Users/cheimann/Documents/io-module-eda/"

diversity <- readr::read_rds(paste(IO_DATA, "diversity/diversity.rds", sep = ""))
genes_norm <- readr::read_rds(paste(IO_DATA,"genes_norm/genes_norm.rds", sep = ""))
genes_norm_log2 <- readr::read_rds(paste(IO_DATA,"genes_norm_log2/genes_norm_log2.rds", sep = ""))
genes_unnorm <- readr::read_rds(paste(IO_DATA,"genes_unnorm/genes_unnorm.rds", sep = ""))
immune_sigs <- readr::read_rds(paste(IO_DATA,"immune_sigs/immune_sigs.rds", sep = ""))
panimmune_sigs <- readr::read_rds(paste(IO_DATA,"panimmune_sigs/panimmune_sigs.rds", sep = ""))
sample <- readr::read_rds(paste(IO_DATA,"sample/sample.rds", sep = ""))

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
                p("This module generates different analysis of response of immune checkpoint inhibitors (ICI) treatment.")
               
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
                            ),
                            column(
                                width = 7,
                                selectizeInput(ns("types"), "Select tumor(s) type(s)", choices = unique(sample$Tissue)),
                                selectizeInput(ns("therapy"), "Select therapy(ies) type(s)", choices = unique(sample$Drug_Pathway)),
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
                    uiOutput(ns("plots")) %>%
                        shinycssloaders::withSpinner()
                ),
                plotBox(
                    width = 12,
                    plotlyOutput(ns("forest")) %>% 
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
        var_choices <- colnames(immune_sigs[3:63])
        selectInput(
            ns("var1_surv"),
            "Variable",
            var_choices,
            selected = "Palmer_BCell"
        )
    })
    
    output$heatmap_op <- renderUI({
        #group_choice <- magrittr::set_names(list(group_internal_choice()), ss_choice())
        var_choices <- colnames(immune_sigs[3:63])
        selectizeInput(
            ns("var2_cox"),
            "Select features",
            var_choices,
            selected = "Palmer_BCell",
            multiple = TRUE
        )
    })

    
    ##Insert the right number of plot output objects into the web page
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

    # Call renderPlot for each one. Plots are only actually generated when they
    # are visible on the web page.
    observe({
        for (i in 1:length(input$datasets)) {
                # Need local so that each item gets its own number. Without it, the value of i in the renderPlot() will be the same across all instances, 
                # because of when the expression is evaluated.
                local({
                    
                    my_i <- i
                    plotname <- input$datasets[my_i]
                    my_var <- input$var1_surv
                    
                    req(!is.null(my_var))
                    survival_data <- sample %>%
                        filter(Dataset == input$datasets[my_i]) %>%
                        select(Sample_ID, Dataset, Treatment, OS_d, OS_e) %>%
                        dplyr::rename(OS = OS_e, OS_time = OS_d)
                    
                    #Var_data
                    var_data <- immune_sigs %>%
                        select(Sample_ID, my_var)
                    
                    outcome <- merge(survival_data, var_data, by = "Sample_ID")
                
                    survival_df <- build_survival_df(
                        df = outcome,
                        group_column = my_var,
                        group_options = "Other",
                        time_column = "OS_time",
                        k = input$divk
                    )
                    
                    survival_df %>%
                        dplyr::group_by(variable) %>%
                        dplyr::summarize(Num1 = sum(status == 1), Num0 = sum(status == 0))
                    
                    fit <- survival::survfit(survival::Surv(time, status) ~ variable, data = survival_df)
                    
                    group_colors <- viridisLite::viridis(input$divk)
                    
                    output[[plotname]] <- renderPlot({
                        
                        
                        create_kmplot(
                            fit = fit,
                            df = survival_df,
                            confint = input$confint,
                            risktable = input$risktable,
                            title = plotname,
                            group_colors = group_colors)
                    })#renderPlot
                })#local
            }#for
         }) #observe
    
    output$forest <- renderPlotly({
        
        survival_data <- sample %>%
            filter(Dataset %in% input$datasets) %>%
            select(Sample_ID, Dataset, Treatment, OS_d, OS_e) %>%
            dplyr::rename(OS = OS_e, OS_time = OS_d)
        
        #Var_data
        var_data <- immune_sigs %>%
            select(Sample_ID, variable = input$var1_surv)
        
        outcome <- merge(survival_data, var_data, by = "Sample_ID")
        
        fit_cox <- function(dataset, data){
            
            data_cox <- data %>% 
                filter(Dataset == dataset)
            
            survival::coxph(survival::Surv(OS_time, OS) ~ (variable), data_cox)
            
        }
        
        
        all_hr <- purrr::map(.x = input$datasets, .f= fit_cox, data = outcome)
        
        names(all_hr) <- input$datasets
        
        meta_stats <- data.frame("Dataset" = names(all_hr), 
                                 "coef"= sapply(all_hr, with, coefficients),
                                 "exp.coef" = sapply(all_hr,  function(l) summary(l)$coef[2]), 
                                 "SE"= sapply(all_hr,  function(l) summary(l)$coef[3]),
                                 "upper" = sapply(all_hr,  function(l) summary(l)$conf.int[3]),
                                 "lower" = sapply(all_hr,  function(l) summary(l)$conf.int[4]))
        
        log_meta_stats <- meta_stats %>% 
            mutate(logHR = log10(exp.coef),
                   logupper = log10(upper),
                   loglower = log10(lower))
        
        create_forestplot(log_meta_stats,
                          x=log_meta_stats$logHR, 
                          y=log_meta_stats$Dataset, 
                          xmin=log_meta_stats$loglower, 
                          xmax=log_meta_stats$logupper, 
                          xintercept = 0, 
                          xlab = 'Hazard Ratio (log10)', 
                          ylab = "Reference")
        
    })
 
}
   