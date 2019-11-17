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
  sample_group_df,
  subset_df, 
  plot_colors
){
  
  ns <- session$ns
  
  output$cellPlot <- renderPlot({
  
  # generate data frames
  fmx_df <- panimmune_data$fmx_df %>% mutate(Tumor_Fraction=1-Stromal_Fraction)
  sample_group_df <- panimmune_data$sample_group_df
  im_expr_df <- panimmune_data$im_expr_df
  group_col <- group_internal_choice()
  group_df <- sample_group_df() %>% dplyr::mutate(Tumor_Fraction=1-Stromal_Fraction)
  
  cell_image_base <- panimmune_data$cell_image_base
  unique_image_variable_ids <- cell_image_base$unique_image_variable_ids
  variable_annotations <- cell_image_base$variable_annotations
  
  ##
  ## The required cell data
  ##
  cois <- get.data.variables(unique.image.variable.ids,variable_annotations,'fmx_df')
  dfc <- build_cellcontent_df(group_df,cois,group_col) 
  dfc <- dfc %>% dplyr::rename(Group=GROUP,Variable=fraction_type,Value=fraction)
  ## Note that ParticipantBarcode is gone.  Each Group,Variable combo simply has instances
  
  ##
  ## The required gene expression data
  ##
  
  ## input unique image variable IDs, get genes with IDs as in expression matrix
  gois <- get.data.variables(unique.image.variable.ids,variable_annotations,'im_expr_df')
  dfg <- build_multi_imageprotein_expression_df(group_df,gois,group_col)  ## dfg$FILTER is the Gene column 
  dfg <- dfg %>% dplyr::select(Group=GROUP,Variable=FILTER,Value=LOG_COUNT)
  ## Note that "ID" aka ParticipantBarcode is gone.  Each Group,Variable combo simply has instances
  
  ### Generate single data frame with all data values
  dfv <- dplyr::bind_rows(dfc, dfg)
  
  #########################################################################
  ##
  ## Variables ranges and summary
  ##
  #########################################################################
  
  ## Mean Value per Group and Variable
  meanz <- dfv %>% dplyr::group_by(Group,Variable) %>% dplyr::summarise(Mean=mean(Value)) 
  ## Max Value for each Variable (includes avg over Group)
  maxz <- dfv %>% dplyr::group_by(Variable) %>% dplyr::summarise(Max=max(Value))
  ## Min Value for each Variable (includes avg over Group)
  minz <- dfv %>% dplyr::group_by(Variable) %>% dplyr::summarise(Min=min(Value)) 
  ## Vector versions
  minvec <- minz %>% purrr:::pluck("Min")
  names(minvec) <- minz %>% purrr::pluck("Variable")
  maxvec <- maxz %>% purrr::pluck("Max")
  names(maxvec) <- maxz %>% purrr::pluck("Variable")
  
  image_grob_start <- cell_image_base$image_grob 
  pathlabels <- cell_image_base$pathlabels
  gTree_name <- cell_image_base$gTree_name ## label of overall gTree object

  fill.color.start <- character(length(pathlabels)) ; names(fill.color.start) <- pathlabels
  for (s in pathlabels){
    fill.color.start[s] <- image_grob_start$children[[gTree_name]]$children[[s]]$gp$fill 
  }
  image_grob <- image_grob_start
  fill.color.new <- character(length(pathlabels)) ; names(fill.color.new) <- pathlabels ## this is for editing
  
  
  #########################################################################
  ##
  ## Get New Colors
  ##
  #########################################################################
  
  sois <- unique(group_df[[group_col]])
  soi <- sois[5]
  
  for (ind in seq(1,length(image.object.labels))){
    ioa <- image.object.labels[ind]
    datavar <- variable_annotations %>% dplyr::filter(ImageVariableID==ioa) %>% purrr::pluck("FeatureLabel")
    colormap <-   variable_annotations %>% dplyr::filter(ImageVariableID==ioa) %>% purrr::pluck("ColorScale")
    fill.color.new[ind] <- getVarColor(datavar,soi,colormap,minvec,maxvec,dfv)
  }
  for (s in pathlabels ){
    image_grob$children[[gTree_name]]$children[[s]]$gp$fill <- fill.color.new[s]
  }

  grid::grid.draw(image_grob)
  
  })
    
}
