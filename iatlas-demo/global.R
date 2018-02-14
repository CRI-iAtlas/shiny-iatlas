library(shinythemes)
library(bigrquery)
library(plotly)
library(heatmaply)
library(survival)
library(survminer)
library(tidyverse)

source("modules/cellcontentmodule.R")
source("modules/immuneinterfacemodule.R")
source("modules/featurecorrelationmodule.R")
source("modules/survivalmodule.R")
#source("functions/load_data.R")
#source("functions/utils.R")

# common plot theme
theme_1012 <- theme(
    axis.text = element_text(face = "bold", size = 10, color = "black"),
    axis.title = element_text(face = "bold", size = 12, color = "black"),
    panel.border = element_rect(colour = "black", size = 1),
    strip.text =  element_text(face = "bold", size = 10, color = "black"),
    strip.background = element_rect(colour = "black", size = 1),
    title = element_text(face = "bold", size = 14, color = "black"),
    legend.text = element_text(face = "bold", size = 8, color = "black")
)

# general data loading & prep
USE_REMOTE = FALSE ## set data as remote (BigQuery) or local(Feature Matrix on disk)

load_cellcontent_data <- function(USE_REMOTE) {
    if (!USE_REMOTE) { 
        load("data/PanImmune_FMx.RData") ## reads in data frame, df. Adjust as needed for local loading
    }
    
    ## Color Maps for Display of Immune Subtypes and TCGA tumors
    subtype_colors <- c("red", "yellow", "green", "cyan", "blue", "magenta")
    names(subtype_colors) <- paste("C", seq(1, 6), sep = "")
    
    rt <- read.table(
        "data/PanCanAtlasTumorsWithHistology-color-coded-2017V2.tsv",
        sep = "\t",
        as.is = T,
        header = T
    )
    tcga_colors <- paste("#", rt$Hex.Colors, sep = "")
    names(tcga_colors) <- rt$Study.Abbreviation
    
    ## Features manifest file is used to map data source and friendly names for display purposes
    rt <- read.table(
        "data/IRWG data manifest - Features.tsv",
        sep = "\t",
        as.is = T,
        header = T
    )
    
    if (!USE_REMOTE) { ## some ugliness as BigQuery transforms "." to "_" on upload for variable names
        fmx.var <- rt$FeatureMatrixLabelTSV
    } else {
        fmx.var <- rt$FeatureMatrixLabelBigQuery
    }  
    friendly.var <-rt$FriendlyLabel
    names(fmx.var) <- friendly.var
    names(friendly.var) <- fmx.var
    
    ## selection choices for the dropdown menu of sample groups
    samplegroups <- c(
        'Subtype_Immune_Model_Based',
        'Study',
        'Subtype_Curated_Malta_Noushmehr_et_al'
    )
    sampleselectionchoices <- friendly.var[samplegroups]
    names(sampleselectionchoices) <- samplegroups
    names(samplegroups) <- sampleselectionchoices
    
    ## selection choices for the cell fractions.  Lots of other choices possible.
    if (!USE_REMOTE) {
        cellcontent <- c(
            'leukocyte_fraction',
            'Lymphocytes.Aggregate1',
            'T.cells.CD8',
            'T_cells_CD4.Aggregate2',
            'Macrophage.Aggregate1'
        )
    } else {
        cellcontent <- c(
            'leukocyte_fraction',
            'Lymphocytes.Aggregate1',
            'T_cells_CD8',
            'T_cells_CD4_Aggregate2',
            'Macrophage_Aggregate1'
        )
    }
    
    cellcontentchoices <- friendly.var[cellcontent]
    names(cellcontent) <- cellcontentchoices
    
    list(df = df,
         tcga_colors = tcga_colors,
         subtype_colors = subtype_colors,
         samplegroups = samplegroups,
         sampleselectionchoices = sampleselectionchoices,
         cellcontent = cellcontent,
         cellcontentchoices = cellcontentchoices)
} 

load_clonaldiversity_data <- function(USE_REMOTE) {
    if (!USE_REMOTE){ 
        load("data/PanImmune_FMx.RData") ## reads in data frame, df. Adjust as needed for local loading
    }
    
    ## Color Maps for Display of Immune Subtypes and TCGA tumors
    subtype_colors <- c("red", "yellow", "green", "cyan", "blue", "magenta") %>% 
        purrr::set_names(paste0("C", seq(1, 6)))
    
    tcga_colors_df <- read_tsv("data/PanCanAtlasTumorsWithHistology-color-coded-2017V2.tsv") %>% 
        select(`Study Abbreviation`, `Hex Colors`) %>% 
        mutate(`Hex Colors` = paste0("#", `Hex Colors`))
    tcga_colors <- tcga_colors_df$`Hex Colors` %>% 
        purrr::set_names(tcga_colors_df$`Study Abbreviation`)
    
    ## Features manifest file is used to map data source and friendly names for
    ## display purposes
    fmx_vars_df <- read_tsv("data/IRWG data manifest - Features.tsv")
    
    fmx_var <- fmx_vars_df$FeatureMatrixLabelTSV %>% 
        purrr::set_names(fmx_vars_df$FriendlyLabel)
    friendly_var <- fmx_vars_df$FriendlyLabel %>% 
        purrr::set_names(fmx_var)
    
    
    ## selection choices for the dropdown menu of sample groups
    sample_groups <- c('Subtype_Immune_Model_Based',
                       'Study',
                       'Subtype_Curated_Malta_Noushmehr_et_al') %>% 
        purrr::set_names(friendly_var[.])
    sample_selection_choices <- friendly_var[sample_groups] %>% 
        purrr::set_names(sample_groups)
    
    
    ## selection choices for diversity metrics
    diversity_metric_choices <- c('Evenness', 'Shannon', 'Richness') %>% 
        purrr::set_names(.)
    
    ## selection choices for receptor type
    receptor_type_choices <- c("TCR", "BCR") %>% 
        purrr::set_names(.)
    
    list(df = df,
         tcga_colors = tcga_colors,
         subtype_colors = subtype_colors,
         sample_groups = sample_groups,
         sample_selection_choices = sample_selection_choices,
         diversity_metric_choices = diversity_metric_choices,
         receptor_type_choices = receptor_type_choices)
}

load_corrheatmap_data <- function(USE_REMOTE) {
    load("data/PanImmune_FMx.RData")
    list(dat = df)
}

cellcontent_data <- load_cellcontent_data(USE_REMOTE)
clonaldiversity_data <- load_clonaldiversity_data(USE_REMOTE)
corrheatmap_data <- load_corrheatmap_data(USE_REMOTE)
survivalcurve_data <- corrheatmap_data


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
