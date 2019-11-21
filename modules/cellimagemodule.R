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

        optionsBox(
          column(
            width = 6,
            uiOutput(ns("select_ui"))
          )
        ),
                
#        selectInput(
#          ns("subtype"),
#          "Subtype",
#          c("C1","C2","C3","C4","C5","C6"),
#          selected = "C5"
#        ),
        
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
  group_display_choice, 
  group_internal_choice,
  study_subset_choice,
  sample_group_df,
  subset_df, 
  plot_colors
){
  
  ns <- session$ns
  
  output$select_ui <- renderUI({
    
    req(
      panimmune_data$sample_group_df,
      group_internal_choice()
    )
    
    if(group_internal_choice() == "Subtype_Curated_Malta_Noushmehr_et_al"){
      req(study_subset_choice())
    }
    
    sample_group_vector <-  panimmune_data$sample_group_df %>% 
      dplyr::filter(sample_group ==  group_internal_choice()) %>% 
      `if`(
        group_internal_choice() == "Subtype_Curated_Malta_Noushmehr_et_al",
        dplyr::filter(., `TCGA Studies`== study_subset_choice()),
        .
      ) %>% dplyr::pull(FeatureValue)
    
    
    selectInput(
      ns("groupselect_method"),
      "Select Group",
      choices = sample_group_vector
    )
    
  })

  output$cellPlot <- renderPlot({
    group_col <- group_internal_choice()
    group_df <- sample_group_df() %>% dplyr::mutate(Tumor_Fraction=1-Stromal_Fraction)
    cell_image_base <- panimmune_data$cell_image_base
    ### Single data frame with all data values
    all_vals_df <- generate_value_df(group_df,group_col,cell_image_base)
    #save(group_df,all_vals_df,cell_image_base,file="look.RData")
    #cat("my choice ",input$groupselect_method,"\n")
    soi <- input$groupselect_method
    image_grob <- get_colored_image(soi,cell_image_base,all_vals_df)
    grid::grid.draw(image_grob)
  })
    
}
