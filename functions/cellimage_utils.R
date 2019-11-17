##
## Get data variables from image variables
## 
get.data.variables <- function(
  image.variables, # variable IDs used in the image
  variable.annotations, # variable annotation file df
  source){ # 'im_expr_df' for expression data or 'fmx_df' for cell data
  variable.annotations %>% 
    dplyr::filter(ImageVariableID %in% image.variables & Source==source) %>%
    purrr::pluck("FeatureLabel") %>% unique()
}

# gene/imageprotein expression functions -------------------------------------------------------

## Like filter_immunomodulator_expression_df but for multiple genes
multi_filter_imageprotein_expression_df <- function(
  df, id_col, filter_col, expression_col, filter_values){
  
  df %>% 
    get_complete_df_by_columns(c( ## in utils.R
      id_col, 
      filter_col, 
      expression_col)) %>% 
    dplyr::select(
      FILTER = filter_col, 
      COUNT = expression_col,
      ID = id_col) %>% 
    dplyr::filter(FILTER %in% filter_values) %>% 
    dplyr::mutate(LOG_COUNT = log10(COUNT + 1)) %>% 
    dplyr::select(ID, FILTER, LOG_COUNT)
}

## This function is based on build_immunomodulator_expression_df

build_multi_imageprotein_expression_df <- function(
  group_df, ## fmx_df row filtered based on availability of sample groups
  genes_needed,  ## gene choices 
  group_col, ## the fmx_df column for the group
  expression_df = panimmune_data$im_expr_df,
  expression_filter_col = "Symbol",
  expression_col = "normalized_count",
  id_col = "ParticipantBarcode"){
  
  # current function was written for narrow format expression data
  # use gather to turn wide input to narrow
  expression_df <- tidyr::gather(expression_df,"Symbol",
                                 "normalized_count",-ParticipantBarcode)
  #expression_df <- tidyr::gather(expression_df,expression_filter_col,
  #                               expression_col,-ParticipantBarcode)
  
  expression_df <- multi_filter_imageprotein_expression_df(
    expression_df, 
    id_col, 
    expression_filter_col,
    expression_col,
    genes_needed)
  expression_df$FILTER <- as.vector(expression_df$FILTER)  ## was a factor
  
  group_df <- group_df %>% 
    get_complete_df_by_columns(c(group_col, id_col)) %>% ## this function is in function/utils.R
    select(GROUP = group_col, ID = id_col)
  
  result_df <- 
    dplyr::inner_join(group_df, expression_df, by = "ID") %>%
    dplyr::select(ID,GROUP, FILTER, LOG_COUNT)

}

# cellcontent functions -------------------------------------------------------

build_cellcontent_df <- function(
  df, 
  cell_columns, ## the cell data columns we need 
  group_column, ## the fmx_df column for the group
  id_col = "ParticipantBarcode"
  ){
  
  assert_df_has_columns(df, c(group_column, cell_columns))
  long_df <- df %>% 
    dplyr::select(
      GROUP = group_column,
      cell_columns) %>% 
    tidyr::drop_na()
  
  if(nrow(long_df) == 0) return(long_df)
  
  result_df <- long_df %>% 
    tidyr::gather(fraction_type, fraction, -GROUP)
  assert_df_has_columns(result_df, c("GROUP", "fraction_type", "fraction"))
  return(result_df)
}


# ------------------ overall input data matrix ---------------------

generate_value_df <- function(
  group_df,
  group_col,
  cell_image_base
  ){
  
  unique_image_variable_ids <- cell_image_base$unique_image_variable_ids
  variable_annotations <- cell_image_base$variable_annotations
  
  ##
  ## The required cell data
  ##
  cois <- get.data.variables(unique_image_variable_ids,variable_annotations,'fmx_df')
  dfc <- build_cellcontent_df(group_df,cois,group_col) 
  dfc <- dfc %>% dplyr::rename(Group=GROUP,Variable=fraction_type,Value=fraction)
  ## No ParticipantBarcode in rows.  Each Group,Variable combo simply has instances
  
  ##
  ## The required gene expression data
  ##
  
  ## input unique image variable IDs, get genes with IDs as in expression matrix
  gois <- get.data.variables(unique_image_variable_ids,variable_annotations,'im_expr_df')
  dfg <- build_multi_imageprotein_expression_df(group_df,gois,group_col)  ## dfg$FILTER is the Gene column 
  dfg <- dfg %>% dplyr::select(Group=GROUP,Variable=FILTER,Value=LOG_COUNT)
  ## Note that "ID" aka ParticipantBarcode is gone.  Each Group,Variable combo simply has instances
  
  ### Generate single data frame with all data values
  dplyr::bind_rows(dfc, dfg)

}

data_ranges <- function(inputdata){
  #########################################################################
  ##
  ## Variables ranges and summary
  ##
  #########################################################################
  
  ## Mean Value per Group and Variable
  meanz <- inputdata %>% dplyr::group_by(Group,Variable) %>% dplyr::summarise(Mean=mean(Value)) 
  ## Max Value for each Variable (includes avg over Group)
  maxz <- inputdata %>% dplyr::group_by(Variable) %>% dplyr::summarise(Max=max(Value))
  ## Min Value for each Variable (includes avg over Group)
  minz <- inputdata %>% dplyr::group_by(Variable) %>% dplyr::summarise(Min=min(Value)) 
  ## Vector versions
  minvec <- minz %>% purrr:::pluck("Min")
  names(minvec) <- minz %>% purrr::pluck("Variable")
  maxvec <- maxz %>% purrr::pluck("Max")
  names(maxvec) <- maxz %>% purrr::pluck("Variable")
  
  list(minvec=minvec,maxvec=maxvec)
}

#--------- color functions -----------

## For the variable of interest, get min max possible values, color range and color value
getVarColor <- function(voi,soi,colormap,minvec,maxvec,dfv){
  vmin <- minvec[voi]
  vmax <- maxvec[voi]
  vnstep <- 51
  vstep <- (vmax-vmin)/(vnstep-1) ## size of step 
  breakList <- seq(vmin,vmax,vstep) 
  allcolors <- colorRampPalette(rev(RColorBrewer::brewer.pal(n = 7,name=colormap)))(length(breakList))
  display.val <- dfv %>% dplyr::filter(Group==soi,Variable==voi) %>% dplyr::select(-Group,-Variable) %>% purrr::pluck("Value") %>% mean()
  b <- display.val
  cind <- min(which(!(b-breakList)>0)) ## right turnover point
  usecolor <- allcolors[cind]
  usecolor
}

get_colored_image <- function(soi,cell_image_base,dfv){

  image_object_labels <- cell_image_base$image_object_labels
  variable_annotations <- cell_image_base$variable_annotations
  image_grob <- cell_image_base$image_grob 
  ##unique_image_variable_ids <- cell_image_base$unique_image_variable_ids
  pathlabels <- cell_image_base$pathlabels
  gTree_name <- cell_image_base$gTree_name ## label of overall gTree object
  
  dfv_ranges <- data_ranges(dfv)
  minvec <- dfv_ranges$minvec
  maxvec <- dfv_ranges$maxvec
  
  fill_color <- character(length(pathlabels)) ; names(fill_color) <- pathlabels ## this is for editing
  
  for (ind in seq(1,length(image_object_labels))){
    ioa <- image_object_labels[ind]
    datavar <- variable_annotations %>% dplyr::filter(ImageVariableID==ioa) %>% purrr::pluck("FeatureLabel")
    colormap <-   variable_annotations %>% dplyr::filter(ImageVariableID==ioa) %>% purrr::pluck("ColorScale")
    fill_color[ind] <- getVarColor(datavar,soi,colormap,minvec,maxvec,dfv)
  }
  for (s in pathlabels ){
    image_grob$children[[gTree_name]]$children[[s]]$gp$fill <- fill_color[s]
  }
  
  image_grob
  
}
