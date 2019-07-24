immunomodulator_UI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    titleBox("iAtlas Explorer — Immunomodulators"),
    textBox(
      width = 12,
      p(stringr::str_c(
        "Explore the expression of genes that code for immunomodulating",
        "proteins, including checkpoint proteins.",
        sep = " "
      ))  
    ),
    
    distributions_plot_module_UI(
      ns("dist"),
      message_html = includeMarkdown("data/markdown/im_dist.markdown"),
      title_text = "Immunomodulator Distributions",
      scale_default = "Log10",
      plot_clicked_group_default = T,
    ),

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
  
  
  data_df <- reactive({
    subset_df() %>% 
      dplyr::select(
        x = group_internal_choice(), 
        "ParticipantBarcode") %>% 
      dplyr::inner_join(panimmune_data$im_expr_df, by = "ParticipantBarcode") %>% 
      dplyr::rename(label = "ParticipantBarcode")
  })
  
  relationship_df <- reactive({
    panimmune_data$im_direct_relationships %>%  
      dplyr::select(
        INTERNAL = `HGNC Symbol`, 
        DISPLAY = Gene,
        `Gene Family`, `Super Category`, `Immune Checkpoint`)
  })
  
  callModule(
    distributions_plot_module,
    "dist",
    "immunomodulators_dist_plot",
    data_df,
    relationship_df,
    sample_group_df,
    plot_colors,
    group_display_choice,
    key_col = "label",
  )

    table_df <- reactive({
      dplyr::select(panimmune_data$im_direct_relationships,-c(X10, Notes))
    })
    
    callModule(data_table_module, "im_table", table_df)
}