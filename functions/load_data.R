load_manifest <- function() {
  if (!USE_REMOTE_GS) {
    feature_table <- read_feather("data/irwg_data-manifest_features.feather")
  } else {
    data_manifest <- gs_title("IRWG data manifest")
    feature_table <- gs_read(ss = data_manifest, ws = "Features")
  }
  return(feature_table)
}

load_data <- function() {
  if (!USE_REMOTE_BQ) {
    df <- read_feather("data/irwg_feature-matrix.feather")
  }

  sample_selection_groups <- create_sample_selection_groups()
  cell_content_groups <- create_cell_content_groups()
  modulators <- load_modulators()

  list(
    df = df,
    tcga_colors = create_tcga_colors(),
    subtype_colors = create_subtype_colors(),
    sample_selection_groups = sample_selection_groups,
    sample_selection_choices = map_chr(sample_selection_groups, get_variable_display_name),
    cell_content_groups = cell_content_groups,
    cell_content_choices = map_chr(cell_content_groups, get_variable_display_name),
    diversity_metric_choices = set_names_to_self(config_yaml$diversity_metric_choices),
    receptor_type_choices = set_names_to_self(config_yaml$receptor_type_choices),
    direct_relationship_modulators = modulators$direct_relationship,
    potential_factors_modulators = modulators$potential_factors,
    immunomodulator_df = create_immunomodulator_df(modulators$direct_relationship)
  )
}

# helper functions ------------------------------------------------------------

load_modulators <- function() {
  if (!USE_REMOTE_GS) {
    df1 <- read_feather("data/irwg_im-genes_direct.feather")
    df2 <- read_feather("data/irwg_im-genes_potential.feather")
  } else {
    data_manifest <- gs_title("Cancer Immunomodulators - TCGA PanImmune Group")
    df1 <- gs_read(ss = data_manifest, ws = "Direct Relationship")
    df2 <- gs_read(ss = data_manifest, ws = "Potential Factors")
  }
  df1 <- set_names(df1, str_replace_all(names(df1), " ", "_"))
  df2 <- set_names(df2, str_replace_all(names(df2), " ", "_"))
  return(list("direct_relationship" = df1, "potential_factors" = df2))
}


## Color Maps for Display of Immune Subtypes and TCGA tumors
create_subtype_colors <- function() {
  c("red", "yellow", "green", "cyan", "blue", "magenta") %>%
    purrr::set_names(paste0("C", seq(1, 6)))
}

create_tcga_colors <- function() {
  tcga_colors_df <- read_feather("data/tcga-pancan_study-table.feather") %>% 
    select(`Study Abbreviation`, `Hex Colors`) %>%
    mutate(`Hex Colors` = paste0("#", `Hex Colors`))
  tcga_colors <- tcga_colors_df$`Hex Colors` %>%
    purrr::set_names(tcga_colors_df$`Study Abbreviation`)
}

## selection choices for the dropdown menu of sample groups
create_sample_selection_groups <- function() {
  choices <- feature_table %>%
    filter(`Variable Class` == "Sample Category") %>%
    use_series(FeatureMatrixLabelTSV)
}

## selection choices for the cell fractions.  Lots of other choices possible.
create_cell_content_groups <- function() {
  if (!USE_REMOTE_BQ) {
    cell_content <- config_yaml$cell_content_local
  } else {
    cell_content <- config_yaml$cell_content_bq
  }
}

create_immunomodulator_df <- function(sample_group, diversity_vars) {
  if (USE_REMOTE_BQ) {
    df <- get_immunomodulator_df_from_bq()
  } else {
    df <- get_immunomodulator_df_from_local()
  }
  return(df)
}

get_immunomodulator_df_from_local <- function() {
  read_feather("data/irwg_im-expr.feather")
}

get_immunomodulator_df_from_bq <- function(direct_relationship_modulators) {
  gene_string <- direct_relationship_modulators %>%
    use_series("HGNC Symbol") %>%
    unique() %>%
    discard(is.na(.)) %>%
    str_c(collapse = "', '") %>%
    str_c("('", ., "')")
  query <- str_c(
    "SELECT ParticipantBarcode, Symbol, normalized_count FROM [isb-cgc-01-0008:Filtered.EBpp_AdjustPANCAN_RNASeqV2_filtered] WHERE Symbol IN ",
    gene_string
  )
  query_exec(query, project = "isb-cgc-01-0008", max_pages = Inf)
}


# immuneinterface helpers -----------------------------------------------------
create_immuneinterface_df <- function(sample_group, diversity_vars) {
  if (USE_REMOTE_BQ) {
    df <- create_immuneinterface_df_from_bq(sample_group, diversity_vars)
  } else {
    df <- create_immuneinterface_df_from_local(sample_group, diversity_vars)
  }
  return(df)
}

create_immuneinterface_df_from_bq <- function(sample_group, diversity_vars) {
  query <- glue::glue(
      "\n
       SELECT {samples}, {vars} \\\n
       FROM [isb-cgc-01-0007:Feature_Matrix.PanImmune_FMx] \\\n
       where {samples} is not null \\\n
       and {vars} is not null \\\n
      ",
      samples = sample_group,
      vars = diversity_vars
  )
  df <- query_exec(query, project = "isb-cgc-01-0007")
}

create_immuneinterface_df_from_local <- function(sample_group, diversity_vars) {
  panimmune_data$df %>%
    select(sample_group, diversity_vars) %>%
    .[complete.cases(.), ] %>%
    gather(metric, diversity, -1) %>%
    separate(metric, into = c("receptor", "metric"), sep = "_")
}
