# UI ----
cellcontent_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    titleBox("Tumor Microenvironment"),
    textBox(
      width = 12,
      p("Explore the immune cell proportions in your sample groups.")  
    ),
    
    # Overall proportions section ----
    sectionBox(
      title = "Overall Cell Proportions",
      messageBox(
        width = 12,
        p("The barplots show the mean proportion of the tumor fraction, overall stromal fraction (one minus tumor fractions) and the leukocyte fraction of samples with each group.  Error bars show standard error of the mean.")
      ),
      fluidRow(
        
        # ** Overall proportions bar plot ----
        plotBox(
          width = 12,
          plotlyOutput(ns("overall_props_barplot")) %>% 
            shinycssloaders::withSpinner()
        )
      ),
      messageBox(
        width = 12,
        p("Click on the bars for a sample group and you will get generate a scatter plot, showing leukocyte fraction on the Y-axis and stromal fraction on the X-axis. Points near the diagonal correspond to tumor samples in which non-tumor stromal cells are nearly all immune cells, and points away from the diagonal correspond to a more mixed or a non-immune stromal tumor microenvironment.  Points in the upper-left triangle of the plot are estimation artifacts."),
        p("Manuscript context:  Looking at TCGA tumor types, select PRAD and then SKCM and you will get what corresponds to Figure 2C.")
      ),
      fluidRow(
        
        # ** Overall proportions correlation plots ----
        plotBox(
          width = 12,
          column(
            width = 6,
            
            plotlyOutput(ns("lf_sf_corr_scatterplot")) %>% 
              shinycssloaders::withSpinner()
          )
        )
      )
    ),
    
    # Cell fractions section ----
    sectionBox(
      title = "Cell Type Fractions",
      messageBox(
        width = 12,
        p("This allows you to draw barplots for the proportion of immune different immune cells in the immune compartment.  The values are estimated by CIBERSORT (“Original” fraction), and various combinations of those estimates are provided, for example “Aggregate 1” corresponding to broader categories of cells, and “Aggregate 2” and “Aggregate 3” to finer categories."), 
        p("Manuscript context:  These bargraphs are similar to Figure 2A, and Figure S2A, but with a different arrangement of bars.")
      ),
      fluidRow(
        optionsBox(
          width = 8,
          selectInput(
            inputId = ns("cf_choice"),
            label = "Select Cell Fraction Type",
            choices = config_yaml$cell_type_aggregates,
            selected = config_yaml$cell_type_aggregates[[1]]
          )
        )
      ),
      fluidRow(
        
        # ** Cell fractions bar plot ----
        plotBox(
          width = 12,
          plotlyOutput(ns("cell_frac_barplot")) %>% 
            shinycssloaders::withSpinner()
        )
      )
    )
  )
}

# Server ----
cellcontent <- function(input, output, session, ss_choice, subset_df) {
  
  ss_internal <- reactive(get_variable_internal_name(ss_choice()))
  
  # Overall proportions logic ----
  plot_colors <- reactive(decide_plot_colors(panimmune_data, ss_internal()))
  
  # ** Overall proportions bar plot render ----
  output$overall_props_barplot <- renderPlotly({
    subset_df() %>% 
      build_tumor_content_df(group_column = ss_internal()) %>% 
      build_barplot_df(
        x_column = "fraction_type",
        y_column = "fraction",
        color_column = ss_internal(),
        operations = c("mean", "se"),
        add_label = TRUE
      ) %>% 
      create_barplot(
        x_column = ss_internal(),
        y_column = "mean", 
        color_column = "fraction_type",
        error_column = "se",
        x_lab = "Fraction type by group",
        y_lab = "Fraction mean",
        source_name = "overall_props_barplot"
      )
  })
  
  # ** Overall proportions scatter plot renders ----
  output$lf_sf_corr_scatterplot <- renderPlotly({
    eventdata <- event_data(
      "plotly_click", source = "overall_props_barplot"
    )
    selected_plot_subgroup <- eventdata$x[[1]]
    validate(
        need(!is.null(eventdata),
             "Click bar plot"),
        need(selected_plot_subgroup %in% extract2(subset_df(), ss_internal()),
             "Click bar plot"))
    
    subset_df() %>%
      build_scatterplot_df(
        filter_column = ss_internal(),
        filter_value = selected_plot_subgroup,
        x_column = "Stromal_Fraction",
        y_column = "leukocyte_fraction"
      ) %>%
      create_scatterplot(
        x_column = "Stromal_Fraction",
        y_column = "leukocyte_fraction",
        x_lab = "Stromal Fraction",
        y_lab = "Leukocyte Fraction",
        title = selected_plot_subgroup,
        corrplot = TRUE
      )
  })
  
  # Cell fractions logic ----
  
  # ** Cell fractions bar plot render ----
  output$cell_frac_barplot <- renderPlotly({
    
    cell_fractions <- as.character(get_variable_group(input$cf_choice))
    subset_df() %>%
      build_cell_fraction_df(
        group_column = ss_internal(), 
        value_columns = cell_fractions
      ) %>%
      build_barplot_df(
        y_column = "fraction",
        x_column = "fraction_type",
        color_column = ss_internal(),
        operations = c("mean", "se"),
        add_label = TRUE
      ) %>% 
      mutate(
        fraction_name = map_chr(
          fraction_type, get_variable_display_name
        )
      ) %>% 
      create_barplot(
        x_column = ss_internal(),
        y_column = "mean",
        color_column = "fraction_name",
        error_column = "se",
        x_lab = "Fraction type by group",
        y_lab = "Fraction mean",
        source_name = "cell_frac_barplot"
      )
  })
}

