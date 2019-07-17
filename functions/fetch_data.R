# Fetch data ----

fetch_manifest <- function() {
  manifest_gs <- googlesheets::gs_title("IRWG data manifest")
  list(
    feature_df = gs_read(
      ss = manifest_gs, ws = "Features"
    ),
    feature_method_df = gs_read(
      ss = manifest_gs, ws = "Feature-Methods Map"
    ),
    immune_subtype_df = gs_read(
      ss = manifest_gs, ws = "Immune Subtype Annotations"
    ),
    tcga_study_df = gs_read(
      ss = manifest_gs, ws = "TCGA Study Annotations"
    ),
    tcga_subtype_df = gs_read(
      ss = manifest_gs, ws = "TCGA Subtype Annotations"
    )
  )
}

fetch_feature_matrix <- function() {
  data_syn_id <- "syn11187757"
  data_file <- synGet(data_syn_id)
  load(data_file$path)
  list(fmx_df = df)
}

fetch_im_annotations <- function() {
  im_annotations_gs <- gs_title(
    "Cancer Immunomodulators - TCGA PanImmune Group"
  )
  list(
    im_direct_relationships = gs_read(
      ss = im_annotations_gs, ws = "Direct Relationship"
    ),
    im_potential_factors = gs_read(
      ss = im_annotations_gs, ws = "Potential Factors"
    )
  )
}

fetch_im_expression <- function(im_direct_relationships) {
  gene_string <- im_direct_relationships %>%
    use_series("HGNC Symbol") %>%
    unique() %>%
    discard(is.na(.)) %>%
    stringr::str_c(collapse = "', '") %>%
    stringr::str_c("('", ., "')")
  query <- stringr::str_c(
    "SELECT ParticipantBarcode, Symbol, normalized_count FROM [isb-cgc-01-0008:Filtered.EBpp_AdjustPANCAN_RNASeqV2_filtered] WHERE Symbol IN ",
    gene_string
  )
  list(
    im_expr_df = bigrquery::query_exec(
      query, project = "isb-cgc-01-0008", max_pages = Inf
    )
  )
}

# Format data ----

format_manifest <- function(manifest_data) {
  list(
    feature_df = manifest_data$feature_df,
    feature_method_df = manifest_data$feature_method_df,
    sample_group_df = manifest_data$tcga_study_df %>% 
      select(-Notes) %>% 
      gather(characteristic, value, 
             -FeatureValue, -FeatureHex, -FeatureName) %>%
      unite(Characteristic, characteristic, value, sep = ": ") %>% 
      group_by(FeatureValue) %>% 
      mutate(Characteristics = stringr::str_c(Characteristic, collapse = "; ")) %>% 
      select(-Characteristic) %>% 
      ungroup() %>% 
      mutate(sample_group = "tcga_study") %>% 
      bind_rows(manifest_data$immune_subtype_df %>% 
                  mutate(sample_group = "immune_subtype")) %>% 
      bind_rows(manifest_data$tcga_subtype_df %>% 
                  mutate(sample_group = "tcga_subtype",
                         `TCGA Studies` = stringr::str_replace_all(
                           `TCGA Studies`, ",", "/"
                         ))) %>% 
      mutate(
        FeatureHex = ifelse(!is.na(FeatureHex),
                            stringr::stringr::str_c("#", FeatureHex),
                            FeatureHex)
      )
  )
}

format_feature_matrix <- function(feature_mat_data) {
  list(
    fmx_df = feature_mat_data$fmx_df %>% 
      mutate_at(
        c("age_at_initial_pathologic_diagnosis", "height", "weight"), 
        as.numeric
        ) %>% 
      mutate_if(is.factor, as.character)
  )
}

format_im_annotations <- function(im_annotations_data) {
  list(
    im_direct_relationships = im_annotations_data$im_direct_relationships,
    im_potential_factors = im_annotations_data$im_potential_factors
  )
}
  
# Save data ----

get_feather_name <- function(obj) {
  stringr::str_c("data/", obj, ".feather")
}

save_local_feather <- function(df_list) {
  iwalk(df_list, ~write_feather(.x, get_feather_name(.y)))
}

# Update local data ----

if (UPDATE_LOCAL) {
  # ** update local manifest data ----
  fetch_manifest() %>% 
    format_manifest() %>% 
    save_local_feather()

  # ** update local feature matrix data ----
  fetch_feature_mat() %>% 
    format_feature_mat() %>% 
    save_local_feather()
  
  # ** update local immunomodulators data ----
  fetch_im_annotations() %>% 
    format_im_annotations() %>% 
    save_local_feather()
}
