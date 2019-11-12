immuneinterface_UI <- function(id) {
    ns <- NS(id)
    
    tagList(
        titleBox("Clonal Diversity by Sample Group"),
        fluidRow(
            optionsBox(width = 4,
                       
                       # Drop-down selected diversity metrics
                       selectInput(
                           inputId = ns("diversity_metric_choice"),
                           label = "Select Receptor Type(s)",
                           choices = "",
                           selected = "Shannon"
                       ),
                       
                       # Checkbox selected receptor type(s)
                       checkboxGroupInput(
                           inputId = ns("receptor_type_choices"),
                           label = "Select Receptor Type(s)",
                           choices = "",
                           selected = "TCR"
                       ),
                       
                       # Checkbox z-score option
                       checkboxInput(
                           inputId = ns("ztransform"),
                           label = "Plot Z-scores",
                           value = FALSE
                       )
            ),
            
            plotBox(width = 8,
                    # Show a plot of the generated distribution
                    plotOutput(outputId = ns("diversityPlot"))
            )
        )
        
    )
}

immuneinterface <- function(
    input, output, session, group_display_choice, group_internal_choice, 
    subset_df, plot_colors) {
    
    output$diversityPlot <- renderPlot({
        
        req(!is.null(subset_df()), cancelOutput = T)
        
        diversity_metric   <- input$diversity_metric_choice
        receptor_types <- input$receptor_type_choices
        
        diversity_vars <- stringr::str_c(receptor_types, diversity_metric,
                                         sep = "_"
        )
        
        ## create dfp, the data frame for plotting, based on choices
        
        plot_df <- subset_df() %>% 
            build_immuneinterface_df(group_internal_choice(), diversity_vars) %>% 
            build_boxplot_df(group_internal_choice(), "diversity")
            
            
            ## adjust scales
            if (diversity_metric %in% c("Evenness", "Richness")) {
                plot_df <- plot_df %>%
                    mutate(diversity = log10(diversity + 1))
                scale_label <- glue::glue("log10({metric}+1)",
                                          metric = diversity_metric
                )
            } else {
                scale_label <- diversity_metric
            }
        
        if (input$ztransform) {
            plot_df <- ztransform_df(plot_df)
            scale_label <- paste0("Z-score: ", scale_label)
        }
        y_label <- glue::glue("Diversity [{label}]", label = scale_label)
        ## custom colors if available
        plot <- build_boxplot(
            plot_df,
            fill_factor = group_internal_choice(),
            x_label = input$selection_choice,
            y_label = y_label,
            fill_colors = plot_colors(),
            facet = "receptor ~ ."
        )
        print(plot)
    })
}

