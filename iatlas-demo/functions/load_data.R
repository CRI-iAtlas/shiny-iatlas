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