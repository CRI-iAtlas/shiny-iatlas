cellimage_UI <-function(id){
  
  ns <- NS(id)
  
  tagList(
    titleBox("iAtlas Explorer — Cell Image"),
    textBox(
      width = 12,
      includeMarkdown("data/markdown/cell_image.markdown")
    ),
    
    sectionBox(
      title = "CIM",
      messageBox(
        width = 12,
        p("Driving instructions")
      ),
      fluidRow(
        
        selectInput(
          ns("subtype"),
          "Subtype",
          c("C1","C2","C3","C4","C5","C6"),
          selected = "C5"
        ),
        
        plotBox(
          width = 8,
          plotOutput(ns("cellPlot"), height = 600) %>%
          shinycssloaders::withSpinner()
        )
      )
    ) # closes sectionBox
    
  ) # closes tagList
}
  
cellimage <- function(
  input, 
  output, 
  session,
  ##ss_choice,
  group_display_choice, 
  group_internal_choice, 
  sample_group_df,
  subset_df, 
  plot_colors
){
  
  ns <- session$ns

  output$cellPlot <- renderPlot({
  
    ##fmx_df <- panimmune_data$fmx_df %>% mutate(Tumor_Fraction=1-Stromal_Fraction)
    sample_group_df <- panimmune_data$sample_group_df
    group_col <- group_internal_choice()
    group_df <- sample_group_df() %>% dplyr::mutate(Tumor_Fraction=1-Stromal_Fraction)
    
    cell_image_base <- panimmune_data$cell_image_base
    
    ### Single data frame with all data values
    dfv <- generate_value_df(group_df,group_col,cell_image_base)
    
    soi <- input$subtype ##sois <- unique(group_df[[group_col]])
    image_grob <- get_colored_image(soi,cell_image_base,dfv)
    grid::grid.draw(image_grob)
    
  })
    
}
