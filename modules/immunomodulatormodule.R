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
      title_text = "Immunomodulator Distributions",
      scale_default = "Log10",
      plot_clicked_group_default = T,
      message_html = p(
        p(stringr::str_c(
          "Select Immumodulator Gene to see its expression in the data set.",
          "Use Select Immumodulator Category (drop-down menu on the right) to",
          "organize the selection by particular categories. The categories",
          "will subsequently appear in the left drop-down menu. The Categories",
          "are:"
        )),
        tags$ul(
          tags$li(em('Gene Family'), ", such as TNF, MHC Class II, Immunoglobulin, or CXC chemokine"), 
          tags$li(em('Super Category'), ", such as Ligand, Receptor, or Antigen Presentation"),
          tags$li(em('Immune Checkpoint'), " classified as  Inhibitory or Stimulatory")
        ),
        p(""),
        p(stringr::str_c(
          "Manuscript context:  If you are looking at Immune Subtypes, select",
          "EDNRB or CXCL10 to get figure 6B."
          )),
        p(stringr::str_c(
          "You can view a histogram for any indvidual distributions by",
          "clicking on its violin plot."
          ))
      ),
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