get.data.variables <- function(
  image.variables, # variable IDs used in the image
  variable.annotations, # variable annotation file df
  source){ # 'im_expr_df' for expression data or 'fmx_df' for cell data
  variable.annotations %>% 
    dplyr::filter(ImageVariableID %in% image.variables & Source==source) %>%
    purrr::pluck("FeatureLabel") %>% unique()
}

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

#########################################################################
##
## Color Functions
##
#########################################################################

## For the variable of interest, get min max possible values, get_cell_image_objectcolor range and color value
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

#########################################################################
##
##Function that takes the user selections and builds the final object for plotting
##
#########################################################################

get_cell_image_object <- function(cellimage_base = panimmune_data$cellimage_base, subtype_selected, vals_for_cellplot){
  ## cellimage_base - Multipart object with all information on the cell image
  cois <- get_cells_from_image(cellimage_base) ## Cells  in the image 
  gois <- get_genes_from_image(cellimage_base) ## Proteins in the image
  
  colnames(vals_for_cellplot) <- c("Variable", "Group", "Value")
 
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


##Network visualization

get_network_object <- function(subtype_selected, nodes, friendly_df = friendly, positions_df = positions, scaffold = cell_scaffold){
  
  ##Edges data
  colnames(scaffold) <- c("source", "target", "interaction") #names required by cyjShiny package
  
  ##Nodes data
  
  #Including FriendlyName annotation
  nodes <- nodes %>% 
    dplyr::mutate(FriendlyName = dplyr::case_when(
      Node %in% friendly_df$Obj ~ friendly_df[Node, 3],
      !(Node %in% friendly_df$Obj) ~ Node
    ))
  
  #include nodes coordinates 
  nodes <- merge(nodes, positions_df, by.x = "Node", by.y = "Variable") 
  
  tbl_nodes <- nodes %>%
    dplyr::filter(Group == subtype_selected) %>% 
    dplyr::rename(id = Node) %>% 
    dplyr::select(id, UpBinRatio, x, y, FriendlyName) %>% 
    dplyr::arrange(id)
  
  cyjShiny::dataFramesToJSON(scaffold, tbl_nodes)
}

