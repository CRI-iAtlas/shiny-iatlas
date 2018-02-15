# look up tables to get sets of column names in the feature matrix.

getVars <- function(var1) {
    if (var1 == "Leukocytes") {
        return(c("Neutrophils.Aggregate2",
                 "Eosinophils.Aggregate2",
                 "Mast_cells.Aggregate2",
                 "Dendritic_cells.Aggregate2",
                 "Macrophage.Aggregate2",
                 "NK_cells.Aggregate2",
                 "B_cells.Aggregate2",
                 "T_cells_CD8.Aggregate2",
                 "T_cells_CD4.Aggregate2"))
    }
    if (var1 == "Gene Sets") {
        return(c("CHANG_CORE_SERUM_RESPONSE_UP",
                 "CSF1_response",
                 "LIexpression_score",
                 "Module3_IFN_score",
                 "TGFB_score_21050467"))
    }
    if (var1 == "Genes RNA-seq & RPPA") {
        return(c( "CD274_RNASeq",
                  "CD68_RNASeq",
                  "CD8A_RNASeq",
                  "CTLA4_RNASeq",
                  "PDCD1_RNASeq",
                  "TREM1_RNASeq",
                  "CD274_RPPA",
                  "CTLA4_RPPA",
                  "LCK_RPPA",
                  "PDCD1_RPPA"))
    }
}

# formatting

getNiceName <- function(x) {
    niceNames <- c(
        "Subtype_Immune_Model_Based"="Immune Subtypes",
        "OS_time"="OS Time", 
        "leukocyte_fraction"="Leukocyte Fraction",
        "mutationrate_nonsilent_per_Mb"="Mutation Rate, Non-Silent",
        "indel_neoantigen_num"="Indel Neoantigens",
        "numberOfImmunogenicMutation"="SNV Neoantigens",
        "StemnessScoreRNA"="Stemness Score RNA",
        "Neutrophils.Aggregate2"="Neutrophils",
        "Eosinophils.Aggregate2"="Eosinophils",
        "Mast_cells.Aggregate2"="Mast cells",
        "Dendritic_cells.Aggregate2"="Dendritic cells",
        "Macrophage.Aggregate2"="Macrophage",
        "NK_cells.Aggregate2"="NK cells",
        "B_cells.Aggregate2"="B cells",
        "T_cells_CD8.Aggregate2"="CD8 T cells",
        "T_cells_CD4.Aggregate2"="CD4 T cells",
        "CHANG_CORE_SERUM_RESPONSE_UP"="Wound response",
        "CSF1_response"="Macrophage",
        "LIexpression_score"="Leukocyte infiltration",
        "Module3_IFN_score"="Interferon gamma",
        "TGFB_score_21050467"="TGF-beta",
        "C1"="C1",
        "C2"="C2",
        "C3"="C3",
        "C4"="C4",
        "C5"="C5",
        "C6"="C6",
        "CD274_RNASeq"="PD1L1 (CD274) RNA-seq",
        "CD68_RNASeq"="CD68 RNA-seq",
        "CD8A_RNASeq"="CD8A RNA-seq",
        "CTLA4_RNASeq"="CTLA4 RNA-seq",
        "PDCD1_RNASeq"="PDCD1 RNA-seq",
        "TREM1_RNASeq"="TREM1 RNA-seq",
        "CD274_RPPA"="PD1L1 (CD274) RPPA",
        "CTLA4_RPPA"="CTLA4 RPPA",
        "LCK_RPPA"="LCK RPPA",
        "PDCD1_RPPA"="PDCD1 RPPA"
    )
    return(niceNames[x])
}

buildDataFrame_corr <- function(dat, var1, var2, catx) {
    getCats <- function(dat, catx) {
        cats <- as.character(na.omit(unique(dat[,catx])))
    }
    
    # get the vectors associated with each term
    cats <- sort(getCats(dat, catx))
    vars <- sort(getVars(var1))
    
    # this would be the correlations
    cormat <- matrix(data=0, ncol=length(cats), nrow=length(vars))
    rownames(cormat) <- vars
    colnames(cormat) <- cats
    # for each factor in catx
    for (ci in cats) {
        # subset dat
        subdat <- dat[dat[,catx] == ci,]
        # compute correlation 
        for (vi in vars) {
            cormat[vi,ci] <- cor(subdat[,vi], subdat[,var2], method="spearman", 
                                 use="pairwise.complete.obs")
        }      
    }  
    # give it nice names
    #colnames(cormat) <- sapply(colnames(cormat), getNiceName)
    rownames(cormat) <- sapply(rownames(cormat), getNiceName)
    cormat[is.na(cormat)] <- 0
    cormat
}

buildDataFrame_surv <- function(dat, var1, timevar, divk) {
    getCats <- function(dat, var1, divk) {
        
        if (var1 %in% c("Subtype_Immune_Model_Based")) {
            # then we don't need to produce catagories.
            cats <- as.character(dat[,var1])
        }
        else {
            cats <- as.character(cut(dat[,var1], divk, ordered_result=T))
        }
        cats
    }
    
    # get the vectors associated with each term
    # if var1 is already a catagory, just use that.
    # otherwise it needs to be divided into divk catagories.
    cats <- getCats(dat, var1, divk)
    
    if (timevar == "OS_time") {
        timevar2 <- "OS"
    } else {
        timevar2 <- "PFI_1"
    }
    
    df <- data.frame(Status = dat[,timevar2], Time = dat[,timevar], 
                     Variable=cats, Measure=dat[,var1])
    df <- na.omit(df)
    print(head(df))
    df
}

get_label_from_data_obj <- function(obj, obj_list, selection){
    obj %>% 
        magrittr::extract2(obj_list) %>% 
        magrittr::extract2(selection) %>% 
        as.character
}

decide_plot_colors <- function(data_obj, sample_group_label){
    color_mapping <- c(
        'Study' = "tcga_colors",
        'Subtype_Immune_Model_Based' = "subtype_colors")
    if(!sample_group_label %in% names(color_mapping)) return(NA)
    color_item  <- magrittr::extract2(color_mapping, sample_group_label)
    plot_colors <- magrittr::extract2(data_obj, color_item)
}
