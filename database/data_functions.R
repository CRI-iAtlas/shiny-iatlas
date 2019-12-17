is_df_empty <- function(df) {
  return(is.null(dim(df)) | dim(df)[1] == 0 | dim(df)[2] == 0)
}

switch_value <- function(current_row, reference_name, field_name, tibble_object = dplyr::tibble()) {
  reference_value <- current_row[[reference_name]]
  current_value <- current_row[[field_name]]
  currect_reference_row <- tibble_object %>%
    dplyr::filter(!!rlang::sym(reference_name) == reference_value)
  if (!.GlobalEnv$is_df_empty(currect_reference_row)) {
    return(currect_reference_row[[field_name]])
  } else if (!is.na(current_value)) {
    return(current_value)
  } else {
    return(NA)
  }
}

link_to_references <- function(current_link) {
  if (!is.na(current_link)) {
    url <- stringi::stri_extract_first(current_link, regex = "(?<=href=\").*?(?=\")")
    if (!identical(url, "NA") & !is.na(url)) {
      return(paste("{", url, "}", sep = ""))
    }
  }
  return(NA)
}

rebuild_features_to_samples <- function(features_to_samples, id, sample_set, samples) {
  if (!is_df_empty(sample_set)) {
    in_joined <- dplyr::inner_join(samples, sample_set, by = c("sample_id" = "sample")) %>% 
      dplyr::select(id, value) %>%
      dplyr::rename_at("id", ~("sample_id")) %>%
      tibble::add_column(feature_id = id %>% as.integer, .before = "sample_id") %>%
      tibble::add_column(inf_value = NA %>% as.integer, .after = "value") %>%
      dplyr::rowwise() %>%
      dplyr::mutate(inf_value = ifelse(is.infinite(value), value, NA), value = ifelse(is.finite(value), value, NA))
    features_to_samples <- dplyr::bind_rows(features_to_samples, in_joined)
  }
  return(features_to_samples)
}

rebuild_genes_to_samples <- function(genes_to_samples, id, sample_set, samples) {
  if (!is_df_empty(sample_set)) {
    in_joined <- samples %>%
      dplyr::inner_join(sample_set, by = c("sample_id" = "sample")) %>%
      dplyr::select(id, status, rna_seq_expr) %>%
      dplyr::rename_at("id", ~("sample_id")) %>%
      tibble::add_column(gene_id = id %>% as.integer, .before = "sample_id")
    return(genes_to_samples %>% dplyr::bind_rows(in_joined))
  }
  return(genes_to_samples)
}

rebuild_gene_relational_data <- function(all_genes, ref_name, field_name = "name", relational_data = dplyr::tibble()) {
  if (is_df_empty(relational_data)) {
    relational_data <- dplyr::tibble() %>%
      tibble::add_column(!!field_name := NA)
  }
  relational_data <- all_genes %>%
    dplyr::select(ref_name) %>%
    dplyr::distinct() %>%
    dplyr::filter(!is.na(!!rlang::sym(ref_name))) %>%
    dplyr::rename_at(ref_name, ~(field_name)) %>%
    dplyr::arrange(!!rlang::sym(field_name))
  return(relational_data)
}

get_ids_from_heirarchy <- function(df, ids, current_id = NULL) {
  if (!missing(current_id) && !is.null(current_id)) {
    ids <- ids %>% dplyr::add_row(id = current_id)
    current_row <- df %>%
      dplyr::select(id, parent) %>%
      dplyr::filter(id == current_id)
    if (!is_df_empty(current_row)) {
      ids <- df %>%
        get_ids_from_heirarchy(ids, current_row$parent) %>%
        dplyr::distinct(id)
    }
  }
  return(ids)
}

rebuild_samples_to_tags <- function(samples_to_tags, id, current_tag_name, sample_set, samples) {
  found = FALSE
  if (current_tag_name %in% sample_set$TCGA_Study) {
    found = TRUE
    current_sample_set <- sample_set %>% dplyr::distinct(sample, TCGA_Study)
  }

  if (current_tag_name %in% sample_set$TCGA_Subtype) {
    found = TRUE
    current_sample_set <- sample_set %>% dplyr::distinct(sample, TCGA_Subtype)
  }

  if (current_tag_name %in% sample_set$Immune_Subtype) {
    found = TRUE
    current_sample_set <- sample_set %>% dplyr::distinct(sample, Immune_Subtype)
  }
  if (isTRUE(found)) {
    current_sample_set %>% purrr::pmap(~{
      current_sample <- ..1
      sample_row <- samples %>% dplyr::filter(sample_id == current_sample)
      if (!is_df_empty(sample_row)) {
        samples_to_tags <<- samples_to_tags %>%
          dplyr::add_row(sample_id = sample_row[["id"]], tag_id = id)
      }
    })
    samples_to_tags <- samples_to_tags %>% dplyr::distinct(sample_id, tag_id)
  }
  return(samples_to_tags)
}
