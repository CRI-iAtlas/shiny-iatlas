##consolidation of scaffold

get_scaffold <- function(node_type, scaffold, dfe_in, cois, gois){
  
  ## identify ligands, receptor, and cells in the scaffold
  node_type_of_node <- node_type$Type 
  names(node_type_of_node) <- node_type$Obj
  ligands <- get_nodes_of_type("Ligand", scaffold, node_type_of_node)
  receptors <- get_nodes_of_type("Receptor", scaffold, node_type_of_node)
  cells <- get_nodes_of_type("Cell", scaffold, node_type_of_node)
  genes <- c(ligands,receptors)
  genes <- intersect(genes, colnames(dfe_in))
  scaffold.nodes <- c(cells,genes)
  
  ## By default all cells and genes are of interest
  #cois <- cells
  #gois <- genes
  # 
  # ## Optional - Read cells or genes of interest
  # #cois <- read_lines("data/cells_of_interest.txt")
  # gois <- read_lines("data/network/immunomodulator_genes.txt")
  # 
  ## Those which we have data for
  gois <- intersect(gois,colnames(dfe_in))
  genes <- gois 
  
  ## If genes or cells of interest are specified, scaffold edge must contain at least one item of interest
  
  ## Filter scaffold and cell and gene list to those of interest
  if ( length(cois)>0 ){ ## needs a better test that acts on a vector
    cells <- intersect(cells,cois)
  }
  if ( length(gois) >0 ){
    genes <- intersect(genes,gois) 
  }
  if ( length(cois)>0 | length(gois) >0 ){
    scaffold.immuno.nodes <- c(cells,genes)
    
    scaffold_cois <- scaffold %>%
      filter(From %in% scaffold.immuno.nodes & To %in% scaffold.immuno.nodes) #filter edges with both nodes of interest (because cois need to be connected to a gois)
    
    scaffold_genes <- scaffold %>% 
      filter(From %in% scaffold.nodes & To %in% scaffold.nodes) %>% #filter edges that we have data on
      filter(!(From %in% cells) & !(To %in% cells)) %>% #we don't want edges with cells here
      filter(From %in% genes | To %in% genes) #edges with at least one gois
    
    scaffold <- dplyr::union(scaffold_cois, scaffold_genes)
  }
  return(scaffold)
}

get_cells_scaffold <- function(
  scaffold,
  node_type
){
  node_type_of_node <- node_type$Type ; names(node_type_of_node) <- node_type$Obj
  cells <- get_nodes_of_type("Cell", scaffold, node_type_of_node)
  cells
}

#subset nodes and edges data based on sample group selection
get_netdata <- function(sample_group, all_net_info, studyImmune = FALSE){
  if(sample_group == "Subtype_Immune_Model_Based"){
    all_net_info$immune
  }else if(sample_group == "Subtype_Curated_Malta_Noushmehr_et_al"){
    sample_data <- all_net_info$subtype
  }else{
    if(studyImmune == TRUE) return(all_net_info$studyImmune)
    else sample_data <- all_net_info$study
  }
}

## this assert function should maybe go in functions/utils.R
assert_list_has_names <- function(lst, names){
  missing_names <- names[!names %in% names(lst)]
  if(length(missing_names) != 0){
    stop("Names/labels not present in the list object: ",
         str_c(missing_names, collapse = ", "))
  }
}

## get nodes of a particular type
## requires scaffold
get_nodes_of_type <- function(type_of_node, scaffold, node_type_of_node){
  unique(sort(c(names(which(node_type_of_node[scaffold$From]==type_of_node)),
              names(which(node_type_of_node[scaffold$To]==type_of_node)))))
}


############################################################################################
##
## Functions for Indiviual Nodes
##
############################################################################################

## helper functions for univariate tertile frequency (should one add asserts to these?)
unibin <- function(df){table(df$Bin)} ## tabulate bin frequencies in Bin column of df
upbinfrac  <- function(v){(as.numeric(v[2]+v[3])/sum(v))} ## for 3-vector, ratio of upper two values to all
keep.node <- function(val,upbinfrac.threshold){val>=upbinfrac.threshold} ## helper function for thresholding upper bin ratio

## Get tertiles for each node
getNodeTertiles <- function(df){
  quantz <- df %>% group_by(Node) %>% summarise(q0=quantile(Value,0,na.rm=T),
                                                   q33=quantile(Value,0.33,na.rm=T),
                                                   q66=quantile(Value,0.66,na.rm=T),
                                                   q1=quantile(Value,1,na.rm=T))
  tertiles <- as.matrix(quantz[,2:5]) ; rownames(tertiles) <- quantz$Node
  tertiles
}

## Bin values according to tertiles
## 
getbin <- function(value,node_label, tertiles){
  cut(value,tertiles[node_label,])
}

## Bin all node values for a particular node
## input
## s: node name
## df: data frame of ParticipantBarcode,Group,Value
## requires tertiles for getbin function
multiBin <- function(s,df,tertiles){
  vals <- df$Value
  beans <- getbin(vals,s, tertiles)
  tibble(ParticipantBarcode=df$ParticipantBarcode,
                Group=df$Group,Bin=beans)
}

## Calculate node inclusion, per Group
## Acts on a df ParticipantBarcode,Group,Bin
## Intermediate steps calculate bin fractions
## Outputs df with Group, and Include, where latter is logical
addPerGroupIncludes <- function(df, bin.threshold){
  df %>% group_by(Group) %>% tidyr::nest() %>% ## split by Group, now have tibble with one row per Group; tibble named data; has ParticipantBarcode, Bin
    dplyr::mutate(BinDistribution=map(data,unibin)) %>% ## add BinDistribution tibble with frequency of samples in each tertile bin
    dplyr::mutate(UpBinRatio=map_dbl(BinDistribution,upbinfrac)) %>% ## add UpBinRatio with ratio of upper two bins to all (three) bins
    dplyr::mutate(Include=map_lgl(.x = UpBinRatio, upbinfrac.threshold = bin.threshold, .f=keep.node)) %>%  ## creat logical for whether node shoud be kept (per group), based on keep.node
    dplyr::select(Group,Include) %>% dplyr::arrange(Group) ## simplify
}


############################################################################################
##
## Functions for Node Pairs
##
############################################################################################

## for two nodes create data frame of bins for both pair members
## requires object ternary
join_binned_pair <- function(
  NodeA,
  NodeB,
  ternary,
  group_column = "Group",
  id_column = "ParticipantBarcode"){
  
  assert_list_has_names(ternary,c(NodeA,NodeB))
  
  dplyr::inner_join(ternary[[NodeA]],ternary[[NodeB]],by=c(id_column,group_column)) %>% 
    dplyr::select(-id_column)

}

tabulate_pair <- function (df){
  df %>% dplyr::group_by(Group) %>% tidyr::nest() %>% dplyr::mutate(t=map(data,table)) %>% dplyr::select(Group,t)
}


## diagnonal over off-diagonal cross-tabulated bin counts 
## uses pseudocount for denominator 
getRatioScore <- function(Xtab,pseudocount=1){
  ratioScore <- (Xtab[3,3] + Xtab[1,1]) / (Xtab[3,1] + Xtab[1,3]+pseudocount)
  if (ratioScore < 1) { 
    ratioScore <- -(1/ratioScore)
  }
  ratioScore
}

getGroupRatioScores <- function(pt){
  pt %>% dplyr::ungroup() %>% dplyr::mutate(ratioScore=map(t,getRatioScore)) %>% 
    dplyr::transmute(Group,ratioScore=as.numeric(ratioScore)) %>%
    dplyr::arrange(Group)
}
##
## Retain concordance ratio if ratio meets concordance threshold and both pair members meet include criterion
##
filterRatioWithIncludes <- function (rscores,from_node,to_node,feature_include,concordance_treshold){ # concordance_treshold=1.62
  from_include <- feature_include[[from_node]]
  to_include <- feature_include[[to_node]]
  b <- inner_join(from_include,to_include,by="Group") %>% mutate(Both=Include.x & Include.y) %>% 
    select(Group,Both)
  g <- function(val,logval){if(logval){val}else{NA}} ## helper for combining two includes
  h <- function(val){if (is.na(val)){NA}else if(val>concordance_treshold){val}else{NA}} # helper for threshold filter
  inner_join(rscores,b,by="Group") %>% 
    mutate(RatioFiltered=map2_dbl(ratioScore,Both,g)) %>% 
    mutate(RatioFiltered=map_dbl(RatioFiltered,h)) %>%
    select(Group,RatioFiltered)
}



## update list of nodes annotation based on user selection
filterNodes <- function(list_edges, annot){
  colnames(annot) <- c("Type", "name")
  annot[annot$name== "CD80"& annot$Type == "Ligand",] <- NA #hardcoded here the fact that CD80 is with two different annotations and this caused a problem in the JSON file
  nodes <- append(list_edges$source, list_edges$target) %>% unique() %>% as.data.frame()
  colnames(nodes) <- "name"
  
  nodes <- merge(nodes, annot, all.x = TRUE) #we want to keep all nodes, even those that do not have annotations
  
  return(nodes)
}


