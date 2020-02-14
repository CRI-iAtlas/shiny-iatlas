#------- consolidation of scaffold

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

#----- consolidating data based on the subset selection

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

## get nodes of a particular type
## requires scaffold
get_nodes_of_type <- function(type_of_node, scaffold, node_type_of_node){
  unique(sort(c(names(which(node_type_of_node[scaffold$From]==type_of_node)),
              names(which(node_type_of_node[scaffold$To]==type_of_node)))))
}

#----function to filter nodes based on abundance threshold

get_nodes <- function(upbin_df, abund_thres, group_subset, byImmune = FALSE, immuneSubtype = NULL){
  
  if(byImmune == FALSE){
    abnodes <- upbin_df %>%
      dplyr::filter(Group %in% group_subset) %>% 
      dplyr::filter(UpBinRatio > (abund_thres/100))%>% 
      dplyr::mutate_if(is.numeric, round, digits = 3)   
    
  }else{
    abnodes <- upbin_df %>%
      dplyr::filter(Group %in% group_subset & Immune %in% immuneSubtype) %>% 
      dplyr::filter(UpBinRatio > (abund_thres/100))%>% 
      dplyr::mutate_if(is.numeric, round, digits = 3)   
  }
  abnodes
}


#----function to filter edges based on concordance threshold and abundant nodes

get_conc_edges <- function(edges_df, ab_nodes, concordance_thres, group_subset, byImmune = FALSE, immuneSubtype = NULL){
  
  if(byImmune == FALSE){
    
    network <- edges_df %>% 
      dplyr::filter(Group %in% group_subset) %>% 
      merge(ab_nodes, by.x = c("From", "Group"), by.y = c("Node", "Group")) %>% #filtering nodes that are abundant
      merge(ab_nodes, by.x = c("To", "Group"), by.y = c("Node", "Group")) %>% #filtering nodes that are abundant
      dplyr::filter(ratioScore > concordance_thres) %>% #filtering concordant edges 
      dplyr::select(From, To, Group, ratioScore) %>%
      dplyr::mutate_if(is.numeric, round, digits = 3)%>% 
      as.data.frame()
    
  }else if(byImmune == TRUE){
    
    network <- edges_df %>% 
      dplyr::filter(Group %in% group_subset & Immune %in% immuneSubtype) %>% 
      merge(ab_nodes, by.x = c("From", "Group", "Immune"), by.y = c("Node", "Group", "Immune")) %>% #filtering nodes that are abundant
      merge(ab_nodes, by.x = c("To", "Group", "Immune"), by.y = c("Node", "Group", "Immune")) %>% #filtering nodes that are abundant
      dplyr::filter(ratioScore > concordance_thres) %>% #filtering concordant edges 
      dplyr::select(From, To, Immune, ratioScore) %>% 
      dplyr::mutate_if(is.numeric, round, digits = 3) %>% 
      as.data.frame() 
  }
  
  network
}

get_ab_nodes <- function(ab_nodes, conc_edges, nodes_annot, byImmune = FALSE){

  all_nodes <- rbind(conc_edges %>% dplyr::select("Node" = "source" , "Group" = "interaction"), 
                     conc_edges %>% dplyr::select("Node" = "target", "Group" = "interaction")) %>% 
    dplyr::distinct()
  
  if(byImmune == FALSE){
    all_nodes <- merge(all_nodes, ab_nodes, by = c("Node", "Group")) 
  }else{
    all_nodes <- merge(all_nodes, ab_nodes, by.x = c("Node", "Group"), by.y = c("Node","Immune")) 
  }
  
  #including the annotation of Friendly Names and types
  all_nodes <- merge(all_nodes, nodes_annot, by.x = "Node", by.y = "Obj") %>% 
                  dplyr::select("Node" = "Node", "Friendly Name" = "FriendlyName", Type, Group, "Abundance" = "UpBinRatio")
  all_nodes
}

get_edge_table <- function(conc_edges, nodes_annot){
  merge(conc_edges, nodes_annot, by.x = "source", by.y = "Obj") %>% 
    dplyr::select("From" = "source", "From (Friendly Name)" = "FriendlyName", target, interaction, score) %>% 
    merge(nodes_annot, by.x = "target", by.y = "Obj") %>%
    dplyr::select(From, `From (Friendly Name)`, "To" = "target", "To (Friendly Name)" = "FriendlyName", "Group" = "interaction", "Concordance" = "score")
  
}
#---- update list of nodes annotation based on user selection (for nodes color)

filterNodes <- function(list_edges, annot){
  colnames(annot) <- c("Type", "name", "FriendlyName", "Gene")
  nodes <- append(list_edges$source, list_edges$target) %>% unique() %>% as.data.frame()
  colnames(nodes) <- "name"
  
  nodes <- merge(nodes, annot, all.x = TRUE) %>% dplyr::arrange(name) #we want to keep all nodes, even those that do not have annotations
  
  return(nodes)
}


#----compute scores for a custom group selection


#------utils functions

## this assert function should maybe go in functions/utils.R
assert_list_has_names <- function(lst, names){
  missing_names <- names[!names %in% names(lst)]
  if(length(missing_names) != 0){
    stop("Names/labels not present in the list object: ",
         stringr::str_c(missing_names, collapse = ", "))
  }
}

## Functions for Indiviual Nodes

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
multiBin <- function(s,df,tertiles, byImmune = FALSE){
  
  vals <- df$Value
  beans <- getbin(vals,s, tertiles)
  
  if(byImmune == FALSE){
    dplyr::tibble(ParticipantBarcode=df$ParticipantBarcode,
           Group=df$Group,Bin=beans)
  }else{
    dplyr::tibble(ParticipantBarcode=df$ParticipantBarcode,
           Group=df$Group, Immune = df$Immune, Bin=beans)
  }
  
}

## Calculate node inclusion, per Group
## Acts on a df ParticipantBarcode,Group,Bin
## Intermediate steps calculate bin fractions
## Outputs df with Group, and Include, where latter is logical
addPerGroupIncludes <- function(df, byImmune = FALSE){
  
  if(byImmune == FALSE){
    ratios <- df %>% group_by(Group) %>% tidyr::nest() %>% ## split by Group, now have tibble with one row per Group; tibble named data; has ParticipantBarcode, Bin
      mutate(BinDistribution=purrr::map(data,unibin)) %>% ## add BinDistribution tibble with frequency of samples in each tertile bin
      mutate(UpBinRatio=purrr::map_dbl(BinDistribution,upbinfrac)) %>% ## add UpBinRatio with ratio of upper two bins to all (three) bins
      select(Group,UpBinRatio) %>% arrange(Group) 
  }else{
    ratios <- df %>% group_by(Group, Immune) %>% tidyr::nest() %>% 
      mutate(BinDistribution=purrr::map(data,unibin)) %>% 
      mutate(UpBinRatio=purrr::map_dbl(BinDistribution,upbinfrac)) %>% 
      select(Group, Immune, UpBinRatio) #%>% arrange(Group)
  }
 
  return(ratios)
}

## Functions for Node Pairs

## for two nodes create data frame of bins for both pair members
## requires object ternary
join_binned_pair <- function(
  NodeA,
  NodeB,
  ternary,
  group_column = "Group",
  id_column = "ParticipantBarcode",
  byImmune = FALSE){
  
  assert_list_has_names(ternary,c(NodeA,NodeB))
  
  if(byImmune == FALSE){
    dplyr::inner_join(ternary[[NodeA]],ternary[[NodeB]],by=c(id_column,group_column)) %>% 
      dplyr::select(-id_column)
  }else{
    dplyr::inner_join(ternary[[NodeA]],ternary[[NodeB]],by=c(id_column,group_column, "Immune")) %>% 
      dplyr::select(-id_column)
  }
  
}

tabulate_pair <- function (df, byImmune = FALSE){
  if(byImmune == FALSE){
    df %>% group_by(Group) %>% tidyr::nest() %>% mutate(t=purrr::map(data,table)) %>% select(Group,t)  
  }else{
    df %>% group_by(Group, Immune) %>% tidyr::nest() %>% mutate(t=purrr::map(data,table)) %>% select(Group,Immune,t)
  }
  
}


## diagonal over off-diagonal cross-tabulated bin counts 
## uses pseudocount for denominator 
getRatioScore <- function(Xtab,pseudocount=1){
  ratioScore <- (Xtab[3,3] + Xtab[1,1]) / (Xtab[3,1] + Xtab[1,3]+pseudocount)
  # if (ratioScore < 1) { 
  #   ratioScore <- -(1/ratioScore)
  # }
  ratioScore
}

getGroupRatioScores <- function(pt, byImmune = FALSE){
  
  if(byImmune == FALSE){
    pt %>% ungroup() %>% mutate(ratioScore=purrr::map(t,getRatioScore)) %>% 
      transmute(Group,ratioScore=as.numeric(ratioScore)) %>%
      arrange(Group)
  }else{
    pt %>% ungroup() %>% mutate(ratioScore=purrr::map(t,getRatioScore)) %>% 
      transmute(Group, Immune, ratioScore=as.numeric(ratioScore)) %>%
      arrange(Group)
  }
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
    mutate(RatioFiltered=purrr::map2_dbl(ratioScore,Both,g)) %>% 
    mutate(RatioFiltered=purrr::map_dbl(RatioFiltered,h)) %>%
    select(Group,RatioFiltered)
}



#-------computing scores for custom groups

get_participants <- function(group_df, 
                             group_col,
                             byImmune = FALSE){
  # The participant list is obtained from the data frame that has the groups

  participants <- group_df$ParticipantBarcode
  groups <- group_df[[group_col]]
  group_of_participant <- groups ; names(group_of_participant) <- participants
  
  return(group_of_participant)
}


get_cell_long <- function(dfc_in, 
                          group_of_participant, 
                          cells,
                          group_df,
                          byImmune = FALSE){
  
  set.seed(42)
  
  participants <- names(group_of_participant)

  dfc <- dfc_in %>% filter(ParticipantBarcode %in% participants) %>%
    select(ParticipantBarcode,paste(cells,".Aggregate2",sep=""))
  colnames(dfc) <- gsub(".Aggregate2","",colnames(dfc))
  dfc <- dfc %>% dplyr::mutate(Group=group_of_participant[ParticipantBarcode])
  
  if(byImmune == FALSE){
    
    dfclong <- dfc %>% tidyr::gather(Cell,Cell_Fraction,-c(ParticipantBarcode,Group)) 
   
  }else{
    immune_group <- get_participants(group_df, "Subtype_Immune_Model_Based")
    dfc <- dfc %>% dplyr::mutate(Immune=immune_group[ParticipantBarcode])
    
    dfclong <- dfc %>% tidyr::gather(Cell,Cell_Fraction,-c(ParticipantBarcode,Group, Immune))
  }

  dfclong <- dfclong %>% dplyr::mutate(Cell_Fraction=Cell_Fraction+rnorm(mean=0, sd=0.0001,nrow(.)))
  
  dfclong.generic <- dfclong %>% rename(Node=Cell,Value=Cell_Fraction)
  
  return(dfclong.generic)
}

get_gene_long <- function(dfe_in, 
                          group_of_participant, 
                          genes,
                          group_df,
                          byImmune = FALSE){
  set.seed(42)
  
  participants <- names(group_of_participant)
  
  dfe <- dfe_in %>% filter(ParticipantBarcode %in% participants) %>% select(ParticipantBarcode,genes)
  dfelong <- dfe %>% tidyr::gather(Gene,Expression,-ParticipantBarcode)
  dfelong <- dfelong %>% dplyr::mutate(ExpLog2 = log2(Expression+1)+rnorm(mean=0, sd=0.0001,nrow(.))) %>%
    dplyr::select(ParticipantBarcode,Gene,Expression=ExpLog2) %>%
    dplyr::mutate(Group=group_of_participant[ParticipantBarcode]) 
  
  if(byImmune == FALSE){
    dfelong <- dfelong %>% 
      dplyr::select(ParticipantBarcode,Group,Gene,Expression)  
  }else{
    immune_group <- get_participants(group_df, "Subtype_Immune_Model_Based")
    
    dfelong <- dfelong %>%
      dplyr::mutate(Immune = immune_group[ParticipantBarcode]) %>% 
      dplyr::select(ParticipantBarcode,Group, Immune,Gene,Expression) 
  }
  
  dfelong.generic <- dfelong %>% rename(Node=Gene,Value=Expression)
  dfelong.generic
}


compute_abundance <- function(subset_df, subset_col, cell_data, expr_data, cois, gois, byImmune = FALSE){
 
  group_participant <- get_participants(subset_df, subset_col, byImmune)
  
  dfclong.generic <- get_cell_long(cell_data, group_participant, cois, subset_df, byImmune)
  dfelong.generic <- get_gene_long(expr_data, group_participant, gois, subset_df, byImmune)
  
  dfn <- dplyr::bind_rows(dfelong.generic, dfclong.generic) 
  
  tertiles <- getNodeTertiles(dfn)
  
  df_ternary_full_info <- dfn %>% dplyr::group_by(Node) %>% tidyr::nest() %>% ## split by nodes
    dplyr::mutate(Bins=purrr::map2(.x = Node,.y = data, tertiles = tertiles, byImmune = byImmune, .f = multiBin)) %>% ## add Bins
    dplyr::mutate(IncludeFeature=purrr::map(.x = Bins, byImmune = byImmune, .f = addPerGroupIncludes))
  
  df_ternary_full_info
  
}

compute_concordance <- function(scaffold, df_ternary_full_info, byImmune){
  ## Lists ternary and feature_include for easier referencing
  ternary <- as.list(df_ternary_full_info$Bins) 
  
  names(ternary) <- df_ternary_full_info$Node ## contains all ternary bins
  
  ## Generate scaffold_edge_score_full_info
  ## Each row contains:
  ## The "From" node
  ## To node
  ## PairBin: tibble with one row per sample, and columns for Group, Bin.x, Bin.y, the bins of pair members
  ## PairTable: tibble with cross-tabulated bin pair frequency for each Group
  ## ratioScores:tibble with concorandance ratio, ratioScore, for each group
  ## uses the join_binned_pair, tabulate_pair, and getGroupRatioScores functions
  scaffold_edge_score_full_info <- scaffold %>%
    dplyr::mutate(PairBin=purrr::map2(.x = From, .y = To, ternary=ternary, byImmune = byImmune, .f=join_binned_pair)) %>% # add table of paired bins per sample
    dplyr::mutate(PairTable=purrr::map(PairBin,byImmune = byImmune,tabulate_pair)) %>% ## add table of paired bin frequencies
    dplyr::mutate(ratioScores=purrr::map(PairTable,byImmune = byImmune,getGroupRatioScores))# %>% ## add concordance ratio
    
 
  scaffold_edge_score <- scaffold_edge_score_full_info %>% dplyr::select(From,To,ratioScores) %>% tidyr::unnest(cols=c(ratioScores))
  
  predicted_network <- scaffold_edge_score# %>% dplyr::filter(!is.na(ratioScores))
  #colnames(predicted_network) <- c("source", "target", "interaction", "score")
  
  predicted_network
  
}


