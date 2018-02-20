load_manifest <- function(){
    if (!USE_REMOTE_GS) {
        feature_table <- read_tsv("data/IRWG data manifest - Features2.tsv")
    } else {
        data_manifest <- gs_title("IRWG data manifest")
        feature_table <- gs_read(ss = data_manifest, ws = "Features")
    }
    return(feature_table)
}

load_data <- function() {
    if (!USE_REMOTE_BQ){ 
        load("data/PanImmune_FMx.RData") ## reads in data frame, df. Adjust as needed for local loading
    }
    
    sample_selection_groups <- create_sample_selection_groups()
    cell_content_groups <- create_cell_content_groups()
    modulators <- load_modulators()

    list(
        df = df,
        tcga_colors = create_tcga_colors(),
        subtype_colors = create_subtype_colors(),
        sample_selection_groups = sample_selection_groups,
        sample_selection_choices = map_chr(sample_selection_groups, get_display_name),
        cell_content_groups = cell_content_groups,
        cell_content_choices = map_chr(cell_content_groups, get_display_name),
        diversity_metric_choices = set_names_to_self(config_yaml$diversity_metric_choices),
        receptor_type_choices = set_names_to_self(config_yaml$receptor_type_choices),
        direct_relationship_modulators = modulators$direct_relationship,
        potential_factors_modulators = modulators$potential_factors)
        
}

# helper functions ------------------------------------------------------------

load_modulators <- function(){
    if (!USE_REMOTE_GS) {
        # df1 <- 
        # df2 <- 
    } else {
        data_manifest <- gs_title("Cancer Immunomodulators - TCGA PanImmune Group")
        df1 <- gs_read(ss = data_manifest, ws = "Direct Relationship")
        df2 <- gs_read(ss = data_manifest, ws = "Potential Factors")
    }
    return(list("direct_relationship" = df1, "potential_factors" = df2))
}


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

## selection choices for the dropdown menu of sample groups
create_sample_selection_groups <- function(){
    choices <- feature_table %>% 
        filter(`Variable Class` == "Sample Category") %>% 
        use_series(FeatureMatrixLabelTSV)
}

## selection choices for the cell fractions.  Lots of other choices possible.
create_cell_content_groups <- function(){
    if (!USE_REMOTE_BQ) {
        cell_content <- config_yaml$cell_content_local
    } else {
        cell_content <- config_yaml$cell_content_bq
    }
}

set_names_to_self <- function(lst){
    set_names(lst, lst)
}


# cellcontent module helpers --------------------------------------------------
create_cellcontent_df <- function(sampgroup, cellcontent){
    if ( USE_REMOTE_BQ) { 
        df <- create_cellcontent_df_from_bq(sampgroup, cellcontent)
    } else {
        df <- create_cellcontent_df_from_local(sampgroup, cellcontent)
    }
    return(df)
}

create_cellcontent_df_from_bq <- function(sampgroup, cellcontent){
    query <- paste(
        'SELECT ',
        sampgroup,
        " , ", 
        cellcontent,
        " FROM [isb-cgc-01-0007:Feature_Matrix.PanImmune_FMx]",
        " where ",
        cellcontent,
        " is not null and ",
        sampgroup,
        " is not null")
    query_exec(query, project = "isb-cgc-01-0007")
}

create_cellcontent_df_from_local <- function(sampgroup, cellcontent){
    panimmune_data$df %>% 
        select(sampgroup, cellcontent) %>% 
        .[complete.cases(.),]
}

# immuneinterface helpers -----------------------------------------------------
create_immuneinterface_df <- function(sample_group, diversity_vars){
    if ( USE_REMOTE_BQ) { 
        df <- create_immuneinterface_df_from_bq(sample_group, diversity_vars)
    } else {
        df <- create_immuneinterface_df_from_local(sample_group, diversity_vars)
    }
    return(df)
}

create_immuneinterface_df_from_bq <- function(sample_group, diversity_vars){
    query <- glue::glue('
                        SELECT {samples}, {vars} \\
                        FROM [isb-cgc-01-0007:Feature_Matrix.PanImmune_FMx] \\
                        where {samples} is not null \\
                        and {vars} is not null \\
                        ',
                        samples = sample_group, 
                        vars = diversity_vars)
    df <- query_exec(query, project = "isb-cgc-01-0007")
}

create_immuneinterface_df_from_local <- function(sample_group, diversity_vars){
    panimmune_data$df %>% 
        select(sample_group, diversity_vars) %>% 
        .[complete.cases(.), ] %>% 
        gather(metric, diversity, -1) %>% 
        separate(metric, into = c("receptor", "metric"), sep = "_")
}
