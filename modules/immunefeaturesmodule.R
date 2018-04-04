immunefeatures_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    titleBox("Immune Feature Trends"),
    textBox(
      width = 12,
      p("This module allows you to see how immune readouts vary across your groups, and how they relate to one another.")  
    ),
    sectionBox(
      title = "Distributions",
      messageBox(
        width = 12,
        p("This displays the value of immune readouts by sample group. Select a variable class to see the distribution of variables within that class displayed as as violin plot."),
        p("Manuscript context: This allows you to display distributions such as those shown in Figures 1C and 1D.")
      ),
      fluidRow(
        optionsBox(
          width = 6,
          selectInput(
            ns("violin_y"),
            "Select violin plot Y variable",
            choices = get_friendly_numeric_columns_by_group()
          )
        )
      ),
      fluidRow(
        plotBox(
          width = 12,
          plotlyOutput(ns("violinPlot")) %>% 
            shinycssloaders::withSpinner()
        )
      )
    ),
    sectionBox(
      title = "Correlations",
      messageBox(
        width = 12,
        p("Here, you can look at correlation of a response variable with other variables, within each sample group.  Select the response variable on the right. Select a variable class on the left to specify which other variable you would like to correlate the response variable with. The result will be a heatmap, with positive correlation shown with a red scale, absence of correlation in white, and negative correlation in blue.  Click on any cell in the heatmap to see the underlying data as a scatterplot. In the scatterplot, each point represents a tumor sample, the response variable is shown on the Y-axis and the row variable is shown on the X-axis.
"),
        p("Manuscript context:  Select “Leukocyte Fraction” as the response variable “DNA Alteration” as the variable class. This will correspond to Figure 4A if you are looking at immune subtypes as your sample grouping.")
      ),
      fluidRow(
        optionsBox(
          width = 12,
          column(
            width = 8,
            selectInput(
              ns("heatmap_y"),
              "Select Variable Class",
              c(
                "Core Expression Signature",
                "DNA Alteration",
                "Adaptive Receptor",
                "T Helper Cell Score",
                "Immune Cell Proportion - Original",
                "Immune Cell Proportion - Aggregate 1",
                "Immune Cell Proportion - Aggregate 2",
                "Immune Cell Proportion - Aggregate 3"
              ),
              selected = "Immune Cell Proportion - Aggregate 2"
            )
          ),
          column(
            width = 4,
            selectInput(
              ns("heatmap_values"),
              "Select response variable",
              c(
                "Leukocyte Fraction" = "leukocyte_fraction",
                "OS Time" = "OS_time",
                "Mutation Rate, Non-Silent" = "mutationrate_nonsilent_per_Mb",
                "Indel Neoantigens" = "indel_neoantigen_num",
                "SNV Neoantigens" = "numberOfImmunogenicMutation",
                "Stemness Score RNA" = "StemnessScoreRNA"
              ),
              selected = "Leukocyte Fraction"
            )
          )
        )
      ),
      fluidRow(
        plotBox(
          width = 6,
          plotlyOutput(ns("corrPlot")) %>% 
            shinycssloaders::withSpinner()
        ),
        plotBox(
          width = 6,
          plotlyOutput(ns("scatterPlot")) %>% 
            shinycssloaders::withSpinner()
        )
      )
    )
  )
}

immunefeatures <- function(input, output, session, ss_choice, subset_df) {
  ns <- session$ns
  
  ss_internal <- reactive(get_variable_internal_name(ss_choice()))
  sample_groups <- reactive(get_category_group(ss_internal()))
  
  
  output$violinPlot <- renderPlotly({
    
    display_x  <- ss_choice()
    display_y  <- input$violin_y
    internal_x <- get_variable_internal_name(display_x)
    internal_y <- get_variable_internal_name(display_y)
    
    plot_df <- subset_df() %>%
      select_(.dots = c(internal_x, internal_y)) %>%
      .[complete.cases(.),]
    
    plot_df %>% 
      create_violinplot(
        internal_x,
        internal_y,
        internal_x,
        xlab = display_x,
        ylab = display_y,
        fill_colors = decide_plot_colors(panimmune_data, internal_x)
      )
  })
  
  hm_variables  <- reactive(
    as.character(get_variable_group(input$heatmap_y))
  )
  
  intermediate_corr_df <- reactive(
    subset_df() %>% 
      build_intermediate_corr_df(
        value_column = input$heatmap_values,
        group_column = ss_internal(),
        group_options = sample_groups(),
        corr_value_columns = hm_variables()
      )
  )
  
  output$corrPlot <- renderPlotly({
    heatmap_corr_mat <- intermediate_corr_df() %>%
      build_heatmap_corr_mat(
        value_column = input$heatmap_values,
        group_column = ss_internal(),
        group_options = sample_groups(),
        corr_value_columns = hm_variables()
      )
    create_heatmap(heatmap_corr_mat, "heatplot")
  })
  
  output$scatterPlot <- renderPlotly({
    eventdata <- event_data("plotly_click", source = "heatplot")
    
    validate(need(
      check_click_data(eventdata, subset_df(), ss_internal(), intermediate_corr_df()),
      "Click heatmap"))
    
    
    internal_variable_name <- eventdata$y[[1]] %>%
      get_variable_internal_name() %>%
      .[. %in% colnames(intermediate_corr_df())]
    
    
    plot_df <- intermediate_corr_df() %>% 
      build_scatterplot_df(
        filter_column = ss_internal(),
        filter_value = eventdata$x[[1]],
        x_column = internal_variable_name,
        y_column = input$heatmap_values
      )
    
    plot_df %>%
      create_scatterplot(
        x_column = internal_variable_name,
        y_column = input$heatmap_values,
        x_lab = eventdata$y[[1]],
        y_lab = get_variable_display_name(input$heatmap_values),
        title = eventdata$x[[1]])
  })
}


check_click_data <- function(eventdata, subset_df, ss_internal, intermediate_corr_df){
  if(is.null(eventdata)) return(FALSE)
  all(eventdata$x[[1]] %in% extract2(subset_df, ss_internal),
      any(get_variable_internal_name(eventdata$y[[1]]) %in% colnames(intermediate_corr_df)))
}

