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
  
  library(grImport2)
  library(grid)
  library(feather)
  library(tidyverse)
  library(RColorBrewer) 

  output$cellPlot <- renderPlot({
  
  gdc <- reactive({group_display_choice()})
  gic <- reactive({group_internal_choice()})
  sgdf <- reactive({sample_group_df()})
  subdf <- reactive({subset_df()})
  reactive({save(gdc,gic,sgdf,subdf,file="see_inside_2.RData")})
  
  fmx_df <- panimmune_data$fmx_df %>% mutate(Tumor_Fraction=1-Stromal_Fraction)
  sample_group_df <- panimmune_data$sample_group_df
  im_expr_df <- panimmune_data$im_expr_df

  ## These come from the app selection
  ## load("see_inside.RData") yields
  ## "group_col"    "group_df"
  ## eg  "Subtype_Curated_Malta_Noushmehr_et_al" and fmx_df filtered to available group annotations 
  
  group_col <- group_internal_choice()
  group_df <- sample_group_df() %>% mutate(Tumor_Fraction=1-Stromal_Fraction)
  
  ## Annotations of image objects
  ## Variable annotations are ImageVariableID, FeatureLabel, Source, ColorScale
  variable.annotations <- read_tsv('data/cell_image_id_annotations.tsv') 
  ## Image obects, in order, labeled in terms of ImageVariableID
  image.object.labels <- read.table('data/cell_image_object_ids.txt',as.is=T)$V1
  ## must be met 
  ## image.object.labels %in% variable.annotations$ImageVariableID
  unique.image.variable.ids <- unique(image.object.labels)
  
  ##
  ## Needed cellular data
  ##
  
  
  cois <- get.data.variables(unique.image.variable.ids,variable.annotations,'fmx_df')
  dfc <- build_cellcontent_df(group_df,cois,group_col) 
  dfc <- dfc %>% rename(Group=GROUP,Variable=fraction_type,Value=fraction)
  ## Note that ParticipantBarcode is gone.  Each Group,Variable combo simply has instances
  
  
  ##
  ## Needed gene expression data
  ##
  
  ## input unique image variable IDs, get genes with IDs as in expression matrix
  gois <- get.data.variables(unique.image.variable.ids,variable.annotations,'im_expr_df')
  dfg <- build_multi_imageprotein_expression_df(group_df,gois,group_col)  ## dfg$FILTER is the Gene column 
  dfg <- dfg %>% select(Group=GROUP,Variable=FILTER,Value=LOG_COUNT)
  ## Note that "ID" aka ParticipantBarcode is gone.  Each Group,Variable combo simply has instances
  
  ### data frame of all values
  
  ## dfc$Variable is chr, dfg$Variable is factor
  dfv <- dplyr::bind_rows(dfc, dfg)
  
  #########################################################################
  ##
  ## Variables ranges and summary
  ##
  #########################################################################
  
  ## Mean Value per Group and Variable
  meanz <- dfv %>% group_by(Group,Variable) %>% summarize(Mean=mean(Value)) 
  ## Max Value for each Variable (includes avg over Group)
  maxz <- dfv %>% group_by(Variable) %>% summarize(Max=max(Value))
  ## Min Value for each Variable (includes avg over Group)
  minz <- dfv %>% group_by(Variable) %>% summarize(Min=min(Value)) 
  ## Vector versions
  minvec <- minz %>% pluck("Min") ; names(minvec) <- minz %>% pluck("Variable")
  maxvec <- maxz %>% pluck("Max") ; names(maxvec) <- maxz %>% pluck("Variable")
  
  #########################################################################
  ##
  ## Get image and convert to grid object
  ##
  #########################################################################
  
  pic <- grImport2::readPicture("data/tcell-svg-take3-cairo.svg")
  #pic <- grImport2::readPicture("data/tcell-start-cairo-edited.svg")
  w <- grImport2::pictureGrob(pic)
  gTree.name <- childNames(w) ## label of overall gTree object
  pathlabels <- w$children[[gTree.name]]$childrenOrder ## labels and order of children 
  fill.color.start <- character(length(pathlabels)) ; names(fill.color.start) <- pathlabels
  for (s in pathlabels){
    fill.color.start[s] <- w$children[[gTree.name]]$children[[s]]$gp$fill 
  }
  wnew <- w
  fill.color.new <- character(length(pathlabels)) ; names(fill.color.new) <- pathlabels ## this is for editing
  
  
  #########################################################################
  ##
  ## Get New Colors
  ##
  #########################################################################
  
  sois <- unique(group_df[[group_col]])
  soi <- "BRCA.LumB"
  soi <- sois[5]
  
  for (ind in seq(1,length(image.object.labels))){
    ioa <- image.object.labels[ind]
    datavar <- variable.annotations %>% filter(ImageVariableID==ioa) %>% pluck("FeatureLabel")
    colormap <-   variable.annotations %>% filter(ImageVariableID==ioa) %>% pluck("ColorScale")
    fill.color.new[ind] <- getVarColor(datavar,soi,colormap)
  }
  for (s in pathlabels ){
    wnew$children[[gTree.name]]$children[[s]]$gp$fill <- fill.color.new[s]
  }

  

  ## #### ---- end paste
  
  
    grid.draw(wnew)
  })
    
}
