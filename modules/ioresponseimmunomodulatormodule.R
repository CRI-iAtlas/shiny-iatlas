ioresponseimmunomodulators_UI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    titleBox("iAtlas Explorer — Immunomodulators"),
    textBox(
      width = 12,
      # p("Explore the ‘omics’ data sets on response to checkpoint inhibitors treatments")
      p("Explore the expression of genes that code for immunomodulating proteins, including checkpoint proteins.")
    ),
    ioresponse_UI(
      ns("dist"),
      message_html = "Select the datasets of interest, an immunomodulator gene, and a criterion to group samples to see the distribution of gene expression within sample groups.
                      Samples can be further divided into additional sub-group based on the availability of those groups for each dataset.  Use the plot parameters to adjust the type of plot and choice of scale.
                      
                      A table with statistical tests comparing all pairwise combinations of groups, for each dataset, is provided at the bottom of the page. You can view a histogram for any individual distributions by clicking on its violin plot. " 
    )
  )
}

ioresponseimmunomodulators <- function(
  input, 
  output, 
  session){
  
  var_choices <- reactive({
    panimmune_data$im_direct_relationships %>%  
      filter(`HGNC Symbol` %in% colnames(ioresponse_data$im_expr)) %>% 
      dplyr::select(
        INTERNAL = `HGNC Symbol`, 
        DISPLAY = Gene,
        CLASS = `Gene Family`
        )
  })
  expr_data <- merge(ioresponse_data$fmx_df, ioresponse_data$im_expr, by = "Sample_ID")
  
  callModule(
    ioresponse,
    "dist",
    "ioimmunomodulator_dist_plot",
    variable_options = var_choices(),
    metadata_feature_df = ioresponse_data$feature_df,
    feature_values = expr_data
  )
}