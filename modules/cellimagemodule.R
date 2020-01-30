cellimage_UI <-function(id){
  
  ns <- NS(id)
  
  tagList(
    titleBox("iAtlas Explorer — Cell Image"),
    textBox(
      width = 12,
      includeMarkdown("data/markdown/cell_image.markdown")
    ),
    
    sectionBox(
      title = "Cell Image Module",
      messageBox(
        width = 12,
        p("Select a subtype to view mean abundance among samples in that type")
      ),
      fluidRow(

        optionsBox(
          column(
            width = 6,
            uiOutput(ns("select_ui"))
          )
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
    cellimage_base <- panimmune_data$cellimage_base
    
    ### Single data frame with all data values
    all_vals_df <- generate_value_df(group_df,group_col,cellimage_base)
    availability <- all_vals_df %>% dplyr::group_by(Group,Variable) %>% dplyr::summarise(Count=length(Value)) %>% 
      dplyr::group_by(Group) %>% dplyr::summarize(MinCount=min(Count))
    
    subtype_selected <- input$groupselect_method
    
    enough_data=TRUE
    if(nrow(availability %>% dplyr::filter(Group==subtype_selected))==0){
      enough_data=FALSE
    }
    if (enough_data==TRUE){
      scount <- availability %>% dplyr::filter(Group==subtype_selected) %>% purrr::pluck("MinCount")
      if (scount <= 3 ){ enough_data=FALSE }
    }
    
    if(enough_data){
      image_grob <- get_colored_image(subtype_selected,cellimage_base,all_vals_df)
      grid::grid.draw(image_grob)
    } else {
      print("Please select another subtype - this one has limited data") ## This needs to go to the app , not to the terminal
    }
    
  })
    
}
