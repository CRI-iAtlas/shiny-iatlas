
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

