cellcontent_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(width = 12, background = "black",
          span(strong("Immune Cell Content by Sample Group"),
               style = "font-size:18px")
      )
    ),
    fluidRow(
      box(width = 4,
          # Drop-down selected sample groups
          selectInput(
            inputId = ns("ss_choice"),
            label = "Select Sample Groups",
            choices = as.character(
              panimmune_data$sample_selection_choices
            ),
            selected = "Immune Subtype"
          ),
          
          # Drop-down selected cell content
          selectInput(
            inputId = ns("cc_choice"),
            label = "Select Cellular Content",
            choices = as.character(
              panimmune_data$cell_content_choices
            ),
            selected = "Leukocyte Fraction"
          )
      ),
      
      box(width = 8,
          # Show a plot of the generated distribution
          plotOutput(ns("distPlot"))
      )
    )
  )
}

cellcontent <- function(input, output, session) {
  output$distPlot <- renderPlot({
    ss_group <- get_variable_internal_name(input$ss_choice)
    cc_group <- get_variable_internal_name(input$cc_choice)
    plot_df <- create_cellcontent_df(ss_group, cc_group)
    plot_colors <- decide_plot_colors(panimmune_data, ss_group)
    plot <- create_boxplot(
      plot_df,
      x = ss_group,
      y = cc_group,
      fill_factor = ss_group,
      x_label = input$ss_choice,
      y_label = input$cc_choice,
      fill_colors = plot_colors
    )
    print(plot)
  })
}
