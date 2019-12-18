#loading data (to be changed to a DB)

IO_DATA="/Users/cheimann/Documents/io-module-eda/"

#diversity <- readr::read_rds(paste(IO_DATA, "diversity/diversity.rds", sep = ""))
#genes_norm <- readr::read_rds(paste(IO_DATA,"genes_norm/genes_norm.rds", sep = ""))
#genes_norm_log2 <- readr::read_rds(paste(IO_DATA,"genes_norm_log2/genes_norm_log2.rds", sep = ""))
#genes_unnorm <- readr::read_rds(paste(IO_DATA,"genes_unnorm/genes_unnorm.rds", sep = ""))
immune_sigs <- readr::read_rds(paste(IO_DATA,"immune_sigs/immune_sigs.rds", sep = ""))
panimmune_sigs <- readr::read_rds(paste(IO_DATA,"panimmune_sigs/panimmune_sigs.rds", sep = ""))
sample <- readr::read_rds(paste(IO_DATA,"sample/sample.rds", sep = ""))

ioresponsemultivariate_UI <- function(id){
    
    ns <- NS(id)
    
    tagList(
        titleBox("iAtlas Explorer —  Immune Checkpoint Inhibitors"),
        textBox(
            width = 12,
            p("Explore the ‘omics’ data sets on response to checkpoint inhibitors treatments")
        ),
        
        sectionBox(
            title = "Multivariate analysis",

            messageBox(
                width = 24,
                p("Multivariate models with different immunogenomics features, in each dataset.")
            ),
            
            optionsBox(
                width = 2, 
                checkboxGroupInput(ns("datasets_mult"), "Select Datasets", choices = c("Gide 2019", "Hugo 2016", 
                                                                                  "IMVigor210", "Prins 2019", "Riaz 2017", "Van Allen 2015"),
                                   selected =  "Gide 2019"), 
                                                # #c("Gide 2019", "Hugo 2016", "IMVigor210", "Prins 2019", "Riaz 2017", "Van Allen 2015")), 
                uiOutput(ns("heatmap_op"))
                ),
            
            plotBox(
                width = 10,
                plotlyOutput(ns("mult_forest"), width = "100%", height = "500px")%>%
                    shinycssloaders::withSpinner()
            )
        )
    )
}

ioresponsemultivariate <- function(input, 
                       output, 
                       session, 
                       group_display_choice, 
                       group_internal_choice,
                       study_subset_choice,
                       sample_group_df,
                       subset_df, 
                       plot_colors){
    
    ns <- session$ns
    
    output$heatmap_op <- renderUI({
        #group_choice <- magrittr::set_names(list(group_internal_choice()), ss_choice())
        var_choices <- colnames(immune_sigs[3:63])
        selectizeInput(
            ns("var2_cox"),
            "Select features",
            var_choices,
            selected = c("Palmer_BCell", "Palmer_CD8"),
            multiple = TRUE
        )
    })

    
    output$mult_forest <- renderPlotly({
        req(input$datasets_mult, input$var2_cox)
            survival_data <- sample %>%
                dplyr::filter(Dataset %in% input$datasets_mult) %>%
                dplyr::select(Sample_ID, Dataset, Treatment, OS_d, OS_e) %>%
                dplyr::rename(OS = OS_e, OS_time = OS_d)
            
            #Var_data
            var_data <- immune_sigs %>%
                dplyr::select(Sample_ID, dplyr::one_of(input$var2_cox))
            
            outcome <- merge(survival_data, var_data, by = "Sample_ID")
           
            cox_features <- as.formula(paste(
                "survival::Surv(OS_time, OS) ~", 
                paste0(input$var2_cox, collapse = " + ")
            ))
            
            fit_cox <- function(dataset, data){
                
                data_cox <- data %>% 
                    filter(Dataset == dataset)
                
                ph <- survival::coxph(cox_features, data_cox)
                #p <- survminer::ggforest(ph, data = data_cox , main = dataset, cpositions = c(0.02, 0.82, 1))
               ph
            }
            
            all_hr <- purrr::map(.x = input$datasets_mult, .f= fit_cox, data = outcome)
            names(all_hr) <- input$datasets_mult
            
            create_ph_df <- function(coxphList){
                
                coef_stats <- as.data.frame(summary(coxphList)$conf.int)
                coef_stats$feature <- row.names(coef_stats)
                coef_stats
            }
            
            cox_coef <- purrr::map(all_hr, create_ph_df)
            cox_df <- data.table::rbindlist(cox_coef, idcol = TRUE)
            
            cox_df <- cox_df %>% 
                mutate(logHR = log10(`exp(coef)`),
                       logupper = log10(`upper .95`),
                       loglower = log10(`lower .95`))
            
            p <- ggplot(cox_df, aes(y=feature, x=logHR, xmin=logupper, xmax=loglower))+
                geom_point(color = 'black')+
                geom_errorbarh(height=.1)+
                geom_vline(xintercept=0, color='black', linetype='dashed')+
                theme_light()+
                facet_wrap(vars(.id))+
                scale_x_continuous(name='Hazard Ratio (log10)')
            
            gp <- plotly::ggplotly(p)
            #Sending a faceted ggplot to plotly makes the x axis name be plotted over the labels. Adjusting it:
            gp[['x']][['layout']][['annotations']][[1]][['y']] <- -0.1
            gp #%>% layout(margin = list(b = 75))
    })
}