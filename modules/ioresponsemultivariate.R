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
                uiOutput(ns("heatmap_op"))
                ),
            
            plotBox(
                width = 10,
                plotlyOutput(ns("mult_forest"), width = "100%", height = "600px")%>%
                    shinycssloaders::withSpinner()
            ),
            plotBox(
                width = 12,
                plotlyOutput(ns("mult_heatmap"), width = "100%", height = "500px")%>%
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
        var_choices <- colnames(fmx_io)
        selectizeInput(
            ns("var2_cox"),
            "Select features",
            var_choices,
            selected = c("CTLA4Th1"),
            multiple = TRUE
        )
    })

    feature_df_mult <- reactive({
        
        req(input$datasets_mult, input$var2_cox)
        
        fmx_io %>% 
            filter(Dataset %in% input$datasets_mult & treatment_when_collected == "Pre") %>%
            select(Sample_ID, Dataset, OS, OS_time, treatment_when_collected, dplyr::one_of(input$var2_cox))
    })
    
    #create dataframe with cox proportional hazard ratio for the selected datasets
    
    mult_ph_df <- reactive({
        req(input$datasets_mult, input$var2_cox)
        
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
        
        all_hr <- purrr::map(.x = input$datasets_mult, .f= fit_cox, data = feature_df_mult())
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
        
        cox_df
    })
    
    output$mult_forest <- renderPlotly({
            
            create_forestplot(mult_ph_df(),
                              x=mult_ph_df()$logHR, 
                              y=mult_ph_df()$feature, 
                              xmin=mult_ph_df()$loglower, 
                              xmax=mult_ph_df()$logupper, 
                              xintercept = 0, 
                              title = "",
                              xlab = 'Hazard Ratio (log10)', 
                              ylab = "Feature",
                              facet = ".id")
    })
    
    output$mult_heatmap <- renderPlotly({
        print(head(mult_ph_df()))

        heatmap_df <- mult_ph_df() %>%
            dplyr::select(.id, feature, logHR) %>%
            tidyr::spread(key = feature, value = logHR)

        row.names(heatmap_df) <- heatmap_df$.id
        heatmap_df$.id <- NULL

        print(head(heatmap_df))

        p <- create_heatmap(t(as.matrix(heatmap_df)), "heatmap", scale_colors = T)
        
        p + geom_tile(size = 1, colour = "black")
    })
}