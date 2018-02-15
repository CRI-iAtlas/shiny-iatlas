load_data <- function(USE_REMOTE) {
    if (!USE_REMOTE){ 
        load("data/PanImmune_FMx.RData") ## reads in data frame, df. Adjust as needed for local loading
    }
    
    friendly_var <- create_friendly_variables()
    sample_groups <- create_sample_groups(friendly_var)
    sample_selection_choices <- create_sample_selection_choices(friendly_var, sample_groups)
    cell_content <- create_cell_content()
    cell_content_choices <- create_cell_content_choices(cell_content, friendly_var)
    names(cell_content) <- cell_content_choices

    list(
        df = df,
        tcga_colors = create_tcga_colors(),
        subtype_colors = create_subtype_colors(),
        sample_groups = sample_groups,
        sample_selection_choices = sample_selection_choices,
        diversity_metric_choices = create_diversity_metric_choices(),
        receptor_type_choices = create_receptor_type_choices(),
        cell_content = cell_content,
        cell_content_choices = cell_content_choices)
}

# helper functions ------------------------------------------------------------

## Color Maps for Display of Immune Subtypes and TCGA tumors
create_subtype_colors <- function(){
    c("red", "yellow", "green", "cyan", "blue", "magenta") %>% 
        purrr::set_names(paste0("C", seq(1, 6)))
}

create_tcga_colors <- function(){
    tcga_colors_df <- read_tsv("data/PanCanAtlasTumorsWithHistology-color-coded-2017V2.tsv") %>% 
        select(`Study Abbreviation`, `Hex Colors`) %>% 
        mutate(`Hex Colors` = paste0("#", `Hex Colors`))
    tcga_colors <- tcga_colors_df$`Hex Colors` %>% 
        purrr::set_names(tcga_colors_df$`Study Abbreviation`)
}

## Features manifest file is used to map data source and friendly names for
## display purposes
create_friendly_variables <- function(){
    fmx_vars_df <- read_tsv("data/IRWG data manifest - Features.tsv")
    fmx_var <- fmx_vars_df$FeatureMatrixLabelTSV %>% 
        purrr::set_names(fmx_vars_df$FriendlyLabel)
    friendly_var <- fmx_vars_df$FriendlyLabel %>% 
        purrr::set_names(fmx_var)
}

## selection choices for the dropdown menu of sample groups
create_sample_groups <- function(friendly_var){
    c('Subtype_Immune_Model_Based',
      'Study',
      'Subtype_Curated_Malta_Noushmehr_et_al') %>% 
        purrr::set_names(friendly_var[.])
}

create_sample_selection_choices <- function(friendly_var, sample_groups){
    sample_selection_choices <- friendly_var[sample_groups] %>% 
        purrr::set_names(sample_groups)
}

## selection choices for diversity metrics
create_diversity_metric_choices <- function(){
    c('Evenness', 'Shannon', 'Richness') %>% 
        purrr::set_names(.)
}

## selection choices for receptor type
create_receptor_type_choices <- function(){
    c("TCR", "BCR") %>% 
        purrr::set_names(.)
}


## selection choices for the cell fractions.  Lots of other choices possible.
create_cell_content <- function(){
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
}

create_cell_content_choices <- function(cell_content, friendly_var){
    cell_content_choices <- friendly_var[cell_content]
    names(cell_content) <- cell_content_choices
}


