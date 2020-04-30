ioresponsefeatures_UI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    titleBox("iAtlas Explorer — Molecular Response to Immune Checkpoint Inhibitors"),
    textBox(
      width = 12,
      p("This module allows you to see how immune readouts vary across your groups.")
    ),
    ioresponse_UI(
      ns("dist"),
      message_html = "This displays the value of immune readouts by sample group. 
      Select the datasets of interest, a variable of interest, and how you want to split your samples into groups. 
      Samples can be further divided into additional sub-group based on the availability of those groups for each dataset.  
      Use the plot parameters to adjust the type of plot and choice of scale.
      
      A table with the result of statistical tests comparing all pairwise combinations of groups is provided at the bottom of 
      the page. For an A vs B comparison, a positive t-statistics corresponds to an elevated value in the A group over group B 
      (Mean value in A greater than mean in B)." 
    )
  )
}

ioresponsefeatures <- function(
  input, 
  output, 
  session){
  
  var_choices <- reactive({
    ioresponse_data$feature_df %>% 
        dplyr::filter(VariableType == "Numeric") %>% 
        dplyr::select(
          INTERNAL = FeatureMatrixLabelTSV, 
          DISPLAY = FriendlyLabel,
          CLASS = `Variable Class`)
  })
  
  
  callModule(
    ioresponse,
    "dist",
    "iofeatures_dist_plot",
    variable_options = var_choices(),
    metadata_feature_df = ioresponse_data$feature_df,
    feature_values = ioresponse_data$fmx_df
  )
}