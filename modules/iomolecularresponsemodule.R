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
                width=12,
                fluidRow(
                    column(
                        width = 2,
                        checkboxGroupInput(ns("datasets"), "Select Datasets", choices = c("Gide 2019", "Hugo 2016", 
                                                                                          "IMVigor210", "Prins 2019", "Riaz 2017", "Van Allen 2015"),
                                           selected = "Gide 2019") 
                    ),
                    
                    
                    column(
                        width = 2,
                        uiOutput(ns("survplot_op")),
                        #checkboxGroupInput(ns("response"), "Select Response Categories", choices = c("OS_d")),
                        #c("OS_d", "OS_y", "OS_e", "PR", "CR", "SD")
                        checkboxInput(ns("confint"), "Confidence Intervals", value = F),
                        checkboxInput(ns("risktable"), "Risk Table", value = T)
                    ),
                    column(
                        width = 2,
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
                        div(class = "form-group shiny-input-container", actionButton(ns("calculate_button"), tags$b("GO"), width = "100%"))
                    )
                    
                )
                
            ),#optionsBox
            plotBox(
                width = 12,
                uiOutput(ns("plots")) %>%
                       shinycssloaders::withSpinner()
            )
           
        )
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
    
    
    
    ##Insert the right number of plot output objects into the web page
    observe({
        output$plots <- renderUI({
           
            plot_output_list <- 
                lapply(1:length(input$datasets), function(i) {
                     #if((i %% 2) != 0){
                         plotname <- input$datasets[i]
                         plotOutput(ns(plotname), height = 400, width = 750)
                     #}  
                    #if((i %% 2) == 0){
                    #    hr()
                    #}
            
            })
    
            # Convert the list to a tagList - this is necessary for the list of items
            # to display properly.
            do.call(tagList, plot_output_list)
        })
    })

    # Call renderPlot for each one. Plots are only actually generated when they
    # are visible on the web page.
    observe({
        for (i in 1:length(input$datasets)) {
                # Need local so that each item gets its own number. Without it, the value
                # of i in the renderPlot() will be the same across all instances, because
                # of when the expression is evaluated.
                local({
                    
                    my_i <- i
                    plotname <- input$datasets[my_i]
                    my_var <- input$var1_surv
                    print(my_var)
                    #outcome <- reactive({
                        req(!is.null(my_var))
                        survival_data <- sample %>%
                            filter(Dataset == input$datasets[my_i]) %>%
                            select(Sample_ID, Dataset, Treatment, OS_d, OS_e) %>%
                            dplyr::rename(OS = OS_e, OS_time = OS_d)
                        
                        #Var_data
                        var_data <- immune_sigs %>%
                            select(Sample_ID, my_var)
                        
                        outcome <- merge(survival_data, var_data, by = "Sample_ID")
                        
                        validate(
                            need(!is.null(var_data), paste("No data available for this feature at the dataset:", input$datasets[my_i]))
                        )
                    
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
    }
   
       
    
#     output$IOsurvPlot <- renderPlot({
#         #req(!is.null(subset_df()), cancelOutput = T)
#         # sample_groups <- get_unique_column_values(group_internal_choice(), subset_df())
#         # n_groups <- dplyr::n_distinct(sample_groups)
# 
#         validate(
#             need(input$var1_surv, "Waiting for input.")
#             # need(dplyr::n_distinct(sample_groups) <= 10 | !input$var1_surv == group_internal_choice(),
#             #      paste0("Too many sample groups (", n_groups, ") for KM plot; ",
#             #             "choose a continuous variable or select different sample groups."))
#         )
# 
#         outcome <- reactive({
# 
#             survival_data <- sample %>%
#                 filter(Dataset %in% input$datasets) %>%
#                 select(Sample_ID, Dataset, Treatment, OS_d, OS_e) %>%
#                 dplyr::rename(OS = OS_e, OS_time = OS_d)
# 
#             #Var_data
#             var_data <- immune_sigs %>%
#                 select(Sample_ID, input$var1_surv)
# 
#             outcome <- merge(survival_data, var_data, by = "Sample_ID")
# 
#             outcome
#         })
# 
# 
#         survival_df <- build_survival_df(
#                 df = outcome(),
#                 group_column = input$var1_surv,
#                 group_options = "Other",
#                 time_column = "OS_time",
#                 k = input$divk
#             )
# 
#         survival_df %>%
#             dplyr::group_by(variable) %>%
#             dplyr::summarize(Num1 = sum(status == 1), Num0 = sum(status == 0))
# 
#         fit <- survival::survfit(survival::Surv(time, status) ~ variable, data = survival_df)
# 
#         title <- input$datasets #get_variable_display_name(input$var1_surv)
#         # if (identical(title, character(0))){
#         #     title <- input$var1_surv
#         # }
#         #
#         # if (title %in% group_options()) {
#         #     group_colors <- plot_colors()
#         #
#         #     if (title == "TCGA Subtype") {
#         #         # ggsurvplot takes the subtype names and pastes the column name onto it 'variable='
#         #         # and colors need to have this modified name.
#         #         group_colors <- group_colors[names(group_colors) %in% survival_df$variable]
#         #     }
#         #
#         #     names(group_colors) <- sapply(names(group_colors), function(a) paste('variable=',a,sep=''))
#         #
#         # } else {
#             group_colors <- viridisLite::viridis(input$divk)
#         #}
# 
#         create_kmplot(
#             fit = fit,
#             df = survival_df,
#             confint = input$confint,
#             risktable = input$risktable,
#             title = title,
#             group_colors = group_colors)
#     })
# 
#     
# }