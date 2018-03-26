immuneinterface_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    titleBox("Clonal Diversity by Sample Group"),
    fluidRow(
      optionsBox(width = 4,
        # Drop-down selected sample groups
        selectInput(
          inputId = ns("selection_choice"),
          label = "Select Sample Groups",
          choices = as.character(panimmune_data$sample_selection_choices),
          selected = "Immune Subtype"
        ),
        
        # Drop-down selected diversity metrics
        selectInput(
          inputId = ns("diversity_metric_choice"),
          label = "Select Receptor Type(s)",
          choices = as.character(panimmune_data$diversity_metric_choices),
          selected = "Shannon"
        ),
        
        # Checkbox selected receptor type(s)
        checkboxGroupInput(
          inputId = ns("receptor_type_choices"),
          label = "Select Receptor Type(s)",
          choices = as.character(panimmune_data$receptor_type_choices),
          selected = "TCR"
        ),
        
        # Checkbox z-score option
        checkboxInput(
          inputId = ns("ztransform"),
          label = "Plot Z-scores",
          value = FALSE
        )
      ),
      
      box(width = 8,
        # Show a plot of the generated distribution
        plotOutput(outputId = ns("diversityPlot"))
      )
    )
    
  )
}

immuneinterface <- function(input, output, session) {
  output$diversityPlot <- renderPlot({
    sample_group_label <- get_variable_internal_name(input$selection_choice)
    diversity_metric <- input$diversity_metric_choice
    receptor_types <- input$receptor_type_choices
    
    diversity_vars <- stringr::str_c(receptor_types, diversity_metric,
                                     sep = "_"
    )
    
    ## create dfp, the data frame for plotting, based on choices
    
    plot_df <- create_immuneinterface_df(sample_group_label, diversity_vars)
    
    
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
    plot_colors <- decide_plot_colors(panimmune_data, sample_group_label)
    plot <- create_boxplot(
      plot_df,
      x = sample_group_label,
      y = "diversity",
      fill_factor = sample_group_label,
      x_label = input$selection_choice,
      y_label = y_label,
      fill_colors = plot_colors,
      facet = "receptor ~ ."
    )
    print(plot)
  })
}

# helper functions ------------------------------------------------------------

ztransform_df <- function(df) {
  df %>%
    group_by(receptor, metric) %>%
    mutate(
      div_mean = mean(diversity),
      div_sd = sd(diversity)
    ) %>%
    ungroup() %>%
    mutate(diversity = (diversity - div_mean) / div_sd)
}
