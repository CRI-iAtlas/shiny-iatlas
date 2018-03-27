cellcontent_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    titleBox("Immune Cell Content by Sample Group"),
    fluidRow(
      textBox(
        width = 12,
        p("`textBox` elements can be used to contain explanatory or descriptive text, in paragraph form.")
      ),
      messageBox(
        width = 12,
        p("`messageBox` elements can be used for highlighting certain points or providing an aside/note.")
      )
    ),
    fluidRow(
      optionsBox(
        width = 4,
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
      
      plotBox(width = 8,
          # Show a plot of the generated distribution
          plotOutput(ns("distPlot"))
      )
    )
  )
}

cellcontent <- function(input, output, session, ss_choice, subset_df) {
  output$distPlot <- renderPlot({
    ss_group <- get_variable_internal_name(ss_choice())
    cc_group <- get_variable_internal_name(input$cc_choice)
    plot_df <- create_cellcontent_df(subset_df(), ss_group, cc_group)
    plot_colors <- decide_plot_colors(panimmune_data, ss_group)
    plot <- create_boxplot(
      plot_df,
      x = ss_group,
      y = cc_group,
      fill_factor = ss_group,
      x_label = ss_choice(),
      y_label = input$cc_choice,
      fill_colors = plot_colors
    )
    print(plot)
  })
}

# cellcontent module helpers --------------------------------------------------

create_cellcontent_df <- function(subset_df, sampgroup, cellcontent) {
    subset_df %>%
        select(sampgroup, cellcontent) %>%
        .[complete.cases(.), ]
}



# create_cellcontent_df <- function(sampgroup, cellcontent) {
#     if (USE_REMOTE_BQ) {
#         df <- create_cellcontent_df_from_bq(sampgroup, cellcontent)
#     } else {
#         df <- create_cellcontent_df_from_local(sampgroup, cellcontent)
#     }
#     return(df)
# }
# 
# create_cellcontent_df_from_bq <- function(sampgroup, cellcontent) {
#     query <- paste(
#         "SELECT ",
#         sampgroup,
#         " , ",
#         cellcontent,
#         " FROM [isb-cgc-01-0007:Feature_Matrix.PanImmune_FMx]",
#         " where ",
#         cellcontent,
#         " is not null and ",
#         sampgroup,
#         " is not null"
#     )
#     query_exec(query, project = "isb-cgc-01-0007")
# }
# 
# 












