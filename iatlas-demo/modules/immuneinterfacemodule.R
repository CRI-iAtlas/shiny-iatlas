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
        
        sample_group <- get_label_from_data_obj(clonaldiversity_data, "sample_groups", input$selection_choice)
        diversity_metric <- input$diversity_metric_choice
        receptor_types <- input$receptor_type_choices
        
        diversity_vars <- stringr::str_c(receptor_types, diversity_metric,
                                         sep = "_")
        
        ## create dfp, the data frame for plotting, based on choices
        dfp <- create_immuneinterface_df(sample_group, diversity_vars)
        
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
        y_label <- glue::glue("Diversity [{label}]", label = scale_label)
        plot <- create_boxplot(dfp, x = sample_group, y = "diversity", fill = sample_group, input$selection_choice, y_label) +
            facet_grid(receptor ~ .)
        ## custom colors if available 
        plot_colors <- decide_plot_colors(clonaldiversity_data, sample_group)
        if(!is.na(plot_colors)){
            plot <- plot + scale_fill_manual(values = plot_colors)}
        print(plot)
    })
}

# helper functions ------------------------------------------------------------


