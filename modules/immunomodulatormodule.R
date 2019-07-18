immunomodulator_UI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    titleBox("iAtlas Explorer — Immunomodulators"),
    textBox(
      width = 12,
      p("Explore the expression of genes that code for immunomodulating proteins, including checkpoint proteins.")  
    ),
    
    immunomodulator_distributions_module_ui(ns("im_dists")),

    data_table_module_UI(
      ns("im_table"),
      title = "Immunomodulator Annotations",
      message_html = p(stringr::str_c(
        "The table shows annotations of the immumodulators, and source.",
        "Use the Search box in the upper right to find an immumodulator of",
        "interest.",
        sep = " "
      ))
    )
  )
}

immunomodulator <- function(
    input, 
    output, 
    session, 
    group_display_choice, 
    group_internal_choice, 
    sample_group_df,
    subset_df, 
    plot_colors
){
    callModule(
        immunomodulator_distributions_module,
        "im_dists",
        group_display_choice, 
        group_internal_choice, 
        sample_group_df,
        subset_df, 
        plot_colors
    )
    
    table_df <- reactive({
      dplyr::select(panimmune_data$im_direct_relationships,-c(X10, Notes))
    })
    
    callModule(data_table_module, "im_table", table_df())
}