immuneinterface_UI <- function(id) {
    
    ns <- NS(id)
    
    tagList(
        fluidPage(
            # Application title
            titlePanel("Clonal Diversity by Sample Group"),
            sidebarLayout(
                sidebarPanel(
                    # Drop-down selected sample groups
                    selectInput(
                        inputId = ns("selection_choice"),
                        label = "Select Sample Groups",
                        choices = as.character(clonaldiversity_data$sample_selection_choices),
                        selected = "Immune Subtype"
                    ), 
                    
                    # Drop-down selected diversity metrics
                    selectInput(
                        inputId = ns("diversity_metric_choice"),
                        label = "Select Receptor Type(s)",
                        choices = as.character(clonaldiversity_data$diversity_metric_choices),
                        selected = "Shannon"
                    ),
                    
                    # Checkbox selected receptor type(s)
                    checkboxGroupInput(
                        inputId = ns("receptor_type_choices"),
                        label = "Select Receptor Type(s)",
                        choices = as.character(clonaldiversity_data$receptor_type_choices),
                        selected = "TCR"
                    ),
                    
                    # Checkbox z-score option
                    checkboxInput(
                        inputId = ns("ztransform"),
                        label = "Plot Z-scores",
                        value = FALSE
                    )
                ),
                
                mainPanel(
                    # Show a plot of the generated distribution
                    plotOutput(outputId = ns("diversityPlot"))        
                )
            )
        )
    )
}

immuneinterface <- function(input, output, session){
    output$diversityPlot <- renderPlot({
        
        sample_group <- clonaldiversity_data$sample_groups[input$selection_choice] ## the label at the data source
        diversity_metric <- input$diversity_metric_choice
        receptor_types <- input$receptor_type_choices
        
        diversity_vars <- stringr::str_c(receptor_types, diversity_metric,
                                         sep = "_")
        
        ## create dfp, the data frame for plotting, based on choices
        if (USE_REMOTE) { 
            bq <- glue::glue('
                             SELECT {samples}, {vars} \\
                             FROM [isb-cgc-01-0007:Feature_Matrix.PanImmune_FMx] \\
                             where {samples} is not null \\
                             and {vars} is not null \\
                             ',
                             samples = sample_group, vars = diversity_vars
            )
            dfp <- query_exec(bq, project="isb-cgc-01-0007") }
        else {
            dfp <- clonaldiversity_data$df %>% 
                select(sample_group, diversity_vars) %>% 
                .[complete.cases(.), ] %>% 
                gather(metric, diversity, -1) %>% 
                separate(metric, into = c("receptor", "metric"), sep = "_")
        }
        
        ## custom colors if available 
        if (sample_group == 'Study') {
            plotcolors <- clonaldiversity_data$tcga_colors
        }
        else if (sample_group == 'Subtype_Immune_Model_Based') {
            plotcolors <- clonaldiversity_data$subtype_colors
        }
        
        ## adjust scales
        if (diversity_metric %in% c("Evenness", "Richness")) {
            dfp <- dfp %>% 
                mutate(diversity = log10(diversity + 1))
            scale_label <- glue::glue("log10({metric}+1)", 
                                      metric = diversity_metric)
        } else {
            scale_label <- diversity_metric
        }
        
        if (input$ztransform) {
            dfp <- dfp %>% 
                group_by(receptor, metric) %>% 
                mutate(div_mean = mean(diversity), 
                       div_sd = sd(diversity)) %>% 
                ungroup() %>% 
                mutate(diversity = (diversity - div_mean) / div_sd)
            scale_label <- paste0("Z-score: ", scale_label)
        }
        
        p <- dfp %>% 
            ggplot(aes_string(x = sample_group, y = "diversity")) + 
            geom_boxplot(aes_string(fill = sample_group)) +
            guides(colour = FALSE, fill = FALSE) +
            ylab(glue::glue("Diversity [{label}]", label = scale_label)) + 
            xlab(input$selection_choice) +
            theme_bw() +
            theme_1012  + 
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
            facet_grid(receptor ~ .)
        if (sample_group %in% c('Study', 'Subtype_Immune_Model_Based')) {
            p <- p + scale_fill_manual(values = plotcolors)
        }
        print(p)
    })
    
    
}