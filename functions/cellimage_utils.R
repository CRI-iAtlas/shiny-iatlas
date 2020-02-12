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

get_cells_from_image <- function(cellimage_base){
    unique_image_variable_ids <- cellimage_base$unique_image_variable_ids
    variable_annotations <- cellimage_base$variable_annotations
    get.data.variables(unique_image_variable_ids,variable_annotations,'fmx_df')
}
get_genes_from_image <- function(cellimage_base){
  unique_image_variable_ids <- cellimage_base$unique_image_variable_ids
  variable_annotations <- cellimage_base$variable_annotations
  get.data.variables(unique_image_variable_ids,variable_annotations,'im_expr_df')
}

generate_value_df <- function(
  group_df,
  group_col,
  cois,
  gois
  ){
  
  ##
  ## The required cell data
  ##
  dfc <- build_cellcontent_df(group_df,cois,group_col) 
  dfc <- dfc %>% dplyr::rename(Group=GROUP,Variable=fraction_type,Value=fraction)
  ## No ParticipantBarcode in rows.  Each Group,Variable combo simply has instances
  
  ##
  ## The required gene expression data
  ##
  
  ## input unique image variable IDs, get genes with IDs as in expression matrix
  dfg <- build_multi_imageprotein_expression_df(group_df,gois,group_col)  ## dfg$FILTER is the Gene column 
  dfg <- dfg %>% dplyr::select(Group=GROUP,Variable=FILTER,Value=LOG_COUNT)
  ## Note that "ID" aka ParticipantBarcode is gone.  Each Group,Variable combo simply has instances
  
  ### Generate single data frame with all data values
  dplyr::bind_rows(dfc, dfg)

}


#########################################################################
##
## Variables ranges and summary
##
#########################################################################

data_ranges <- function(inputdata){
  
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
  
  summary_stats <- inputdata %>% dplyr::group_by(Group,Variable) %>% 
    dplyr::summarise(Mean=mean(Value),SD=sd(Value)) %>%
    dplyr::mutate(MeanPlusSD=Mean+SD,MeanMinusSD=Mean-SD)
  
  bounds <- summary_stats %>% dplyr::group_by(Variable) %>% 
    dplyr::summarise(MaxBound=max(MeanPlusSD),MinBound=min(MeanMinusSD))
      
  list(minvec=minvec,maxvec=maxvec,meanz=meanz,summary_stats=summary_stats,bounds=bounds)
}

#########################################################################
##
## Color Functions
##
#########################################################################

## For the variable of interest, get min max possible values, color range and color value
getVarColor <- function(voi,soi,colormap,value_df,range_df,alpha=1.){

  alpha.hex <- toupper(as.hexmode(0:255))[round(alpha*255)+1]
  
  display.val <- value_df %>% dplyr::filter(Group==soi,Variable==voi) %>% purrr::pluck("Value")
  vmin <- range_df %>% dplyr::filter(Variable==voi) %>% purrr::pluck("MinBound")
  vmax <- range_df %>% dplyr::filter(Variable==voi) %>% purrr::pluck("MaxBound")
  vnstep <- 51
  vstep <- (vmax-vmin)/(vnstep-1) ## size of step
  if(vstep==0 || is.na(vstep)){
    stop("step size in getVarColor is 0 or NA")
  }
  if (length(display.val) == 0 || is.na(display.val)){
    stop("Display value: ",display.val," is problematic")
  }
  breakList <- seq(vmin,vmax,vstep) 
  cind <- min(which(!(display.val-breakList)>0)) ## right turnover point
  
  allcolors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = 7,name=colormap))(length(breakList))
  allcolors.with.alpha <- paste(allcolors,alpha.hex,sep = "")
      
  usecolor <- allcolors.with.alpha[cind]
  usecolor
}

get_colored_image <- function(soi,cellimage_base,value_df,range_df){

  image_object_labels <- cellimage_base$image_object_labels
  variable_annotations <- cellimage_base$variable_annotations
  image_grob <- cellimage_base$image_grob
  pathlabels <- cellimage_base$pathlabels
  gTree_name <- cellimage_base$gTree_name ## label of overall gTree object
  
  #dfv_ranges <- data_ranges(dfv)

  fill_color <- character(length(pathlabels[1:44])) ; names(fill_color) <- pathlabels[1:44] ## this is for editing
  
  for (ind in seq(1,length(image_object_labels))){
    ioa <- image_object_labels[ind]
    datavar <- variable_annotations %>% dplyr::filter(ImageVariableID==ioa) %>% purrr::pluck("FeatureLabel")
    colormap <-   variable_annotations %>% dplyr::filter(ImageVariableID==ioa) %>% purrr::pluck("ColorScale")
    alpha <-   variable_annotations %>% dplyr::filter(ImageVariableID==ioa) %>% purrr::pluck("alpha")
    fill_color[ind] <- getVarColor(datavar,soi,colormap,value_df,range_df,alpha)
  }
  for (s in pathlabels[1:44] ){
    image_grob$children[[gTree_name]]$children[[s]]$gp$fill <- fill_color[s]
  }
  
  image_grob
  
}

##Function that takes the user selections and builds the final object for plotting

get_cell_image_object <- function(cellimage_base = panimmune_data$cellimage_base, subtype_selected, vals_for_cellplot){
  ## cellimage_base - Multipart object with all information on the cell image
  cois <- get_cells_from_image(cellimage_base) ## Cells  in the image 
  gois <- get_genes_from_image(cellimage_base) ## Proteins in the image
  
  vals_for_cellplot$Variable <- gsub("Macrophage.Aggregate1", "Macrophage.Aggregate2",  vals_for_cellplot$Variable)
  ### Before proceeding with plot, obtain vals_for_cellplot and ranges_for_cellplot
  
  ## vals_for_cellplot
  ### Columns are 
  ### Group: the subtype
  ### Variable: the cell or gene variable
  ### Value: the value in that subtype and for that variable, either by the averaging, or abundance ratio
  
  ## ranges_for_cellplot
  ### Group: the subtype
  ### Variable: the cell or gene variable
  ### MinBound and Maxbound: the lower and upper range of values to correspond with the color range
  
  ranges_for_cellplot <- tibble::tibble(Variable=c(cois,gois),MinBound=0,MaxBound=1)
  
  image_grob <- get_colored_image(subtype_selected,cellimage_base,vals_for_cellplot,ranges_for_cellplot)
  image_grob
}
