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
  
  #### WORK HERE
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

## not sure why this is here .Seems like it is already in functions/transform.R and should be deleted
#build_cell_fraction_df <- function(df, group_column, value_columns){
#  assert_df_has_columns(df, c(group_column, value_columns))
#  result_df <- df %>% 
#    dplyr::select(GROUP = group_column, value_columns) %>% 
#    tidyr::gather(fraction_type, fraction, -GROUP) %>% 
#    tidyr::drop_na()
#  assert_df_has_columns(result_df, c("GROUP", "fraction_type", "fraction"))
#  return(result_df)
#}

#--------- color functions -----------


## For the variable of interest, get min max possible values, color range and color value
getVarColor <- function(voi,soi,colormap){
  vmin <- minvec[voi]
  vmax <- maxvec[voi]
  vnstep <- 51
  vstep <- (vmax-vmin)/(vnstep-1) ## size of step 
  breakList <- seq(vmin,vmax,vstep) 
  allcolors <- colorRampPalette(rev(brewer.pal(n = 7,name=colormap)))(length(breakList))
  display.val <- dfv %>% dplyr::filter(Group==soi,Variable==voi) %>% dplyr::select(-Group,-Variable) %>% purrr::pluck("Value") %>% mean()
  b <- display.val
  cind <- min(which(!(b-breakList)>0)) ## right turnover point
  usecolor <- allcolors[cind]
  usecolor
}
