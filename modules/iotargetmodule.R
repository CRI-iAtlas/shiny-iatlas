iotarget_UI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    titleBox("iAtlas Explorer — IO Targets"),
    textBox(
      width = 12,
      includeMarkdown("data/markdown/io_target.markdown")
    ),
    
    distributions_plot_module_UI(
      ns("dist"),
      message_html = includeMarkdown("data/markdown/io_target_dist.markdown"),
      title_text = "IO Target Gene Expression Distributions",
      scale_default = "Log10",
      plot_clicked_group_default = T,
    ),
    
    data_table_module_UI(
      ns("io_table"), 
      title = "IO Target Annotations",
      message_html = p(stringr::str_c(
        "The table shows annotations of the IO Targets, with columns as",
        "described above and description based on public resources such as",
        "NCBI. Use the Search box in the upper right to find an IO target of",
        "interest.",
        "\n",
        "The last column provides a direct link to target information on the",
        "IO Landscape resource such as number of target agents under active",
        "development, and development stage.",
        sep = " "
      ))
    )
    
    
  )
}

iotarget <- function(
  input, 
  output, 
  session,
  group_display_choice, 
  group_internal_choice, 
  sample_group_df,
  subset_df, 
  plot_colors
) {
  
  ns <- session$ns
  
  url_gene <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    gene  <- query[['gene']]
    if (!is.null(gene)) {
      url_gene <- gene
    } else {
      url_gene <- NA
    }
    return(url_gene)
  })
  
  data_df <- reactive({
    subset_df() %>%
      dplyr::select(
        x = group_internal_choice(),
        "ParticipantBarcode") %>% 
      dplyr::inner_join(panimmune_data$io_target_expr_df, by = "ParticipantBarcode") %>% 
      dplyr::rename(label = "ParticipantBarcode")
  })
  
  metadata_df <- reactive({
    panimmune_data$io_target_annotations %>%  
      dplyr::select(
        INTERNAL = `HGNC Symbol`, 
        DISPLAY = `Gene`,
        Pathway, `Therapy Type`) 
  })
  
  callModule(
    distributions_plot_module,
    "dist",
    "io_targets_dist_plot",
    data_df,
    metadata_df,
    sample_group_df,
    plot_colors,
    group_display_choice,
    variable_selection_default = url_gene(),
    key_col = "label",
  )

  table_df <- reactive({
    panimmune_data$io_target_annotations %>% 
      dplyr::mutate(LinkText = .$IO_target_URL%>% 
                      stringr::str_split(";") %>% 
                      purrr::map(rev) %>% 
                      purrr::map_chr(1)
      ) %>% 
      dplyr::mutate(
        `Link to IO Landscape`= paste(
          "<a href=\"",
          IO_target_URL,"\">",
          LinkText,"</a>", 
          sep=""
        )
      ) %>% 
      dplyr::select(-IO_target_URL, -LinkText)
  })       
  
  
  callModule(data_table_module, "io_table", table_df, escape = F)
  
}