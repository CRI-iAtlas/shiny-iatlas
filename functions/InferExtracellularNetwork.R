library(tidyverse)
#source("scripts/extracellnet_utils.R")


get_scaffold <- function(node_type, scaffold, dfe_in, gois){
  
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
  cois <- cells
  #gois <- genes
  # 
  # ## Optional - Read cells or genes of interest
  # #cois <- read_lines("data/cells_of_interest.txt")
  # gois <- read_lines("data/network/immunomodulator_genes.txt")
  # 
  # gois <- c(gois, "CXCL10")
  # 
  ## Those which we have data for
  gois <- intersect(gois,colnames(dfe_in))
  genes <- gois 
  
  ## If genes or cells of interest are specified, scaffold edge must connect two items of interst
  
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

# get_participants <- function(group_df, 
#                              group_col){
#   # The participant list is obtained from the data frame that has the groups
#   participants <- group_df$ParticipantBarcode
#   groups <- group_df[[group_col]]
#   group_of_participant <- groups ; names(group_of_participant) <- participants
#   return(group_of_participant)
# }

# get_cell_long <- function(dfc_in, 
#                           group_of_participant, 
#                           cells){
#   
#   participants <- names(group_of_participant)
#   
#   dfc <- dfc_in %>% filter(ParticipantBarcode %in% participants) %>%
#     select(ParticipantBarcode,paste(cells,".Aggregate2",sep=""))
#   colnames(dfc) <- gsub(".Aggregate2","",colnames(dfc))
#   dfc <- dfc %>% mutate(Group=group_of_participant[ParticipantBarcode])
#   dfclong <- dfc %>% tidyr::gather(Cell,Cell_Fraction,-c(ParticipantBarcode,Group))
#   dfclong <- dfclong %>% mutate(Cell_Fraction=Cell_Fraction+rnorm(mean=0, sd=0.0001,nrow(.)))
#   
#   dfclong.generic <- dfclong %>% rename(Node=Cell,Value=Cell_Fraction)
#   
#   return(dfclong.generic)
# }

# get_gene_long <- function(dfe_in, 
#                           group_of_participant, 
#                           genes){
#   ## Process gene expression values to yield df with ParticipantBarcode,Group, Gene and Expression
#   ## original code has a noise  addition rnorm(mean=0, sd=0.00001, n=nrow(jtab))
#   participants <- names(group_of_participant)
#   
#   dfe <- dfe_in %>% filter(ParticipantBarcode %in% participants) %>% select(ParticipantBarcode,genes)
#   dfelong <- dfe %>% tidyr::gather(Gene,Expression,-ParticipantBarcode)
#   dfelong <- dfelong %>% mutate(ExpLog2 = log2(Expression+1)+rnorm(mean=0, sd=0.0001,nrow(.))) %>%
#     select(ParticipantBarcode,Gene,Expression=ExpLog2) %>%
#     mutate(Group=group_of_participant[ParticipantBarcode]) %>%
#     select(ParticipantBarcode,Group,Gene,Expression)
#   ## check why this last step seems slow -
# 
#   dfelong.generic <- dfelong %>% rename(Node=Gene,Value=Expression)
#   dfelong.generic
# }


# get_abundant_nodes <- function(df_ternary, ab_threshold){
#   
#   #tertiles <- getNodeTertiles(dfn)
#   
#   ## Generate df_ternary_full_info
#   ## Each row contains a Node, with tibbles for data, Bins, IncludeFeature
#   ## data: ParticipantBarcode,Group,Value
#   ## Bins: ParticipantBarcode,Group,Bin, which has the bin for each value
#   ## IncludeFeature: Group, Include, based on inclusion criteria
#   # df_ternary_full_info <- dfn %>% group_by(Node) %>% nest() %>% ## split by nodes
#   # mutate(Bins=map2(.x = Node,.y = data, tertiles = tertiles, .f = multiBin)) %>% ## add Bins
#   #    mutate(IncludeFeature=map(.x = Bins, bin.threshold = ab_threshold, .f = addPerGroupIncludes)) ## add IncludeFeature
#   
#   df_ternary_full_info <-  df_ternary %>%
#     dplyr::mutate(IncludeFeature=purrr::map(.x = Bins, bin.threshold = ab_threshold, .f = addPerGroupIncludes)) ## add IncludeFeature
#   
#   df_ternary_full_info
# }

# get_network <- function(scaffold, df_ternary_full_info, ternary, conc_threshold){
#   ## Lists ternary and feature_include for easier referencing
#   #ternary <- as.list(df_ternary_full_info$Bins) 
#   #names(ternary) <- df_ternary_full_info$Node ## contains all ternary bins
#   feature_include <- as.list(df_ternary_full_info$IncludeFeature) ; names(feature_include) <- df_ternary_full_info$Node ## contains include/exclude flags

  ## Generate scaffold_edge_score_full_info
  ## Each row contains:
  ## The "From" node
  ## To node
  ## PairBin: tibble with one row per sample, and columns for Group, Bin.x, Bin.y, the bins of pair members
  ## PairTable: tibble with cross-tabulated bin pair frequency for each Group
  ## ratioScores:tibble with concorandance ratio, ratioScore, for each group
  ## FilteredRatio: as ratioScores, but NA if inclusion criteria are not met
  ## uses the join_binned_pair, tabulate_pair, and getGroupRatioScores functions
#   scaffold_edge_score_full_info <- scaffold %>%
#     dplyr::mutate(PairBin=purrr::map2(.x = From, .y = To, ternary=ternary, .f=join_binned_pair)) %>% # add table of paired bins per sample
#     dplyr::mutate(PairTable=purrr::map(PairBin,tabulate_pair)) %>% ## add table of paired bin frequencies
#     dplyr:: mutate(ratioScores=purrr::map(PairTable,getGroupRatioScores)) %>% ## add concordance ratio
#     dplyr::mutate(FilteredRatio=purrr::pmap(.l=list(ratioScores,From,To), feature_include = feature_include, concordance_treshold = conc_threshold, .f=filterRatioWithIncludes)) ## flag ratios for removal with NA
# 
#   ## group-specfic edge scores, with NAs where not meeting criteria
#   scaffold_edge_score <- scaffold_edge_score_full_info %>% select(From,To,FilteredRatio) %>% unnest() %>% rename(Score=RatioFiltered)
# 
#   predicted_network <- scaffold_edge_score %>% filter(!is.na(Score))
#   colnames(predicted_network) <- c("source", "target", "interaction", "score")
#   
#   predicted_network
# 
# }
