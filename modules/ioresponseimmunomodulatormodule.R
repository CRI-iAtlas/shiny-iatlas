ioresponseimmunomodulators_UI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    titleBox("iAtlas Explorer — Molecular Response to Immune Checkpoint Inhibitors"),
    textBox(
      width = 12,
      # p("Explore the ‘omics’ data sets on response to checkpoint inhibitors treatments")
      p("This module allows you to see how immune readouts vary across your groups, and how they relate to one another.")
    ),
    ioresponse_UI(
      ns("dist"),
      message_html = "This displays the value of immune readouts by sample group. 
          Select the datasets of interest, a criteria to group samples and a variable class to see the distribution of variables within that class displayed as a violin plot. Samples can be further divided in extra groups, for each dataset independently.
          A table with statistical tests comparing all pairwise comparison of groups, for each dataset, is provided at the bottom of the page." #includeMarkdown("data/markdown/immune_features_dist.markdown")
    )
  )
}

ioresponseimmunomodulators <- function(
  input, 
  output, 
  session, 
  group_display_choice,
  group_internal_choice,
  study_subset_choice,
  sample_group_df,
  subset_df,
  plot_colors){
  
  var_choices <- reactive({
    panimmune_data$im_direct_relationships %>%  
      filter(`HGNC Symbol` %in% colnames(im_expr_io_df)) %>% 
      dplyr::select(
        INTERNAL = `HGNC Symbol`, 
        DISPLAY = Gene,
        CLASS = `Gene Family`
        )
  })
  expr_data <- merge(fmx_io, im_expr_io_df, by = "Sample_ID")
  
  callModule(
    ioresponse,
    "dist",
    "ioimmunomodulator_dist_plot",
    variable_options = var_choices(),
    metadata_feature_df = feature_io_df,
    feature_values = expr_data
  )
}