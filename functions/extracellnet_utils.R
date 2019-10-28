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


#----function to filter edges based on concordance threshold

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
#---- update list of nodes annotation based on user selection (for coloring nodes)

filterNodes <- function(list_edges, annot){
  colnames(annot) <- c("Type", "name")
  annot[annot$name== "CD80"& annot$Type == "Ligand",] <- NA #hardcoded here the fact that CD80 is with two different annotations and this caused a problem in the JSON file
  nodes <- append(list_edges$source, list_edges$target) %>% unique() %>% as.data.frame()
  colnames(nodes) <- "name"
  
  nodes <- merge(nodes, annot, all.x = TRUE) #we want to keep all nodes, even those that do not have annotations
  
  return(nodes)
}


