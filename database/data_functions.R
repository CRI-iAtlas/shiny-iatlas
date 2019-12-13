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

value_to_id <- function(reference, current_value, value, reference_match) {
  if (identical(reference, reference_match)) {
    return(value)
  } else if (!is.na(current_value)) {
    return(current_value)
  } else {
    return(NA)
  }
}

rebuild_features <- function(features, classes, method_tags) {
  # Ensure data.frames.
  classes <- classes %>% as.data.frame
  method_tags <- method_tags %>% as.data.frame
  for (row in 1:nrow(classes)) {
    features <- features %>%
      dplyr::select(dplyr::everything()) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(class_int = value_to_id(class, class_int, classes[row, "id"], classes[row, "name"]))
  }
  for (row in 1:nrow(method_tags)) {
    features <- features %>%
      dplyr::select(dplyr::everything()) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(mt_int = value_to_id(methods_tag, mt_int, method_tags[row, "id"], method_tags[row, "name"]))
  }
  return(features)
}

update_features_to_samples <- function(features_to_samples, id, feature_value, features) {
  # Ensure data.frames.
  features <- features %>% as.data.frame
  if (!is_df_empty(features)) {
    for (row in 1:nrow(features)) {
      features_to_samples <- features_to_samples %>%
        dplyr::select(dplyr::everything()) %>%
        dplyr::rowwise() %>%
        dplyr::add_row(sample_id = id, feature_id = features[row, "id"], value = feature_value)
    }
  }
  return(features_to_samples)
}

rebuild_features_to_samples <- function(features_to_samples, id, current_sample_id, all_samples, features) {
  current_sample_set <- all_samples %>%
    dplyr::select(sample, feature, value) %>%
    dplyr::filter(sample == current_sample_id) %>%
    as.data.frame
  if (!is_df_empty(current_sample_set)) {
    for (row in 1:nrow(current_sample_set)) {
      current_features <- features %>%
        dplyr::select(id, name) %>%
        dplyr::filter(name == current_sample_set[row, "feature"])
      features_to_samples <- features_to_samples %>% update_features_to_samples(id, current_sample_set[row, "value"], current_features)
    }
  }
  return(features_to_samples)
}

rebuild_genes <- function(genes,
                          super_categories,
                          gene_families,
                          immune_checkpoints,
                          gene_functions,
                          pathways,
                          therapy_types) {
  # Ensure data.frames.
  super_categories <- super_categories %>% as.data.frame
  gene_families <- gene_families %>% as.data.frame
  immune_checkpoints <- immune_checkpoints %>% as.data.frame
  gene_functions <- gene_functions %>% as.data.frame
  pathways <- pathways %>% as.data.frame
  therapy_types <- therapy_types %>% as.data.frame
  for (row in 1:nrow(super_categories)) {
    genes <- genes %>%
      dplyr::select(dplyr::everything()) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(sc_int = value_to_id(super_category, sc_int, super_categories[row, "id"], super_categories[row, "name"]))
  }
  cat("Added super_category ids to genes.", fill = TRUE)
  for (row in 1:nrow(gene_families)) {
    genes <- genes %>%
      dplyr::select(dplyr::everything()) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(gene_family_int = value_to_id(gene_family, gene_family_int, gene_families[row, "id"], gene_families[row, "name"]))
  }
  cat("Added gene_family ids to genes.", fill = TRUE)
  for (row in 1:nrow(immune_checkpoints)) {
    genes <- genes %>%
      dplyr::select(dplyr::everything()) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(immune_checkpoint_int = value_to_id(immune_checkpoint, immune_checkpoint_int, immune_checkpoints[row, "id"], immune_checkpoints[row, "name"]))
  }
  cat("Added immune_checkpoint ids to genes.", fill = TRUE)
  for (row in 1:nrow(gene_functions)) {
    genes <- genes %>%
      dplyr::select(dplyr::everything()) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(gene_function_int = value_to_id(gene_function, gene_function_int, gene_functions[row, "id"], gene_functions[row, "name"]))
  }
  cat("Added gene_function ids to genes.", fill = TRUE)
  for (row in 1:nrow(pathways)) {
    genes <- genes %>%
      dplyr::select(dplyr::everything()) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(pathway_int = value_to_id(pathway, pathway_int, pathways[row, "id"], pathways[row, "name"]))
  }
  cat("Added pathway ids to genes.", fill = TRUE)
  for (row in 1:nrow(therapy_types)) {
    genes <- genes %>%
      dplyr::select(dplyr::everything()) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(therapy_type_int = value_to_id(therapy_type, therapy_type_int, therapy_types[row, "id"], therapy_types[row, "name"]))
  }
  cat("Added therapy_type ids to genes.", fill = TRUE)
  return(genes)
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

rebuild_tags <- function(tags, all_tags) {
  # Ensure data.frames.
  all_tags <- all_tags %>%
    dplyr::select(id:name) %>%
    as.data.frame
  for (row in 1:nrow(all_tags)) {
    tags <- tags %>%
      dplyr::select(dplyr::everything()) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(parent = value_to_id(parent_group, parent, all_tags[row, "id"], all_tags[row, "name"])) %>%
      dplyr::mutate(subgroup = value_to_id(subtype_group, subgroup, all_tags[row, "id"], all_tags[row, "name"]))
  }
  return(tags)
}

get_ids_from_heirarchy <- function(df, ids, current_id = NULL) {
  if (!missing(current_id) && !is.null(current_id)) {
    ids <- ids %>% dplyr::add_row(id = current_id)
    current_row <- df %>%
      dplyr::select(id:parent) %>%
      dplyr::filter(id == current_id)
    if (!is_df_empty(current_row)) {
      ids <- df %>%
        get_ids_from_heirarchy(ids, current_row$parent) %>%
        dplyr::distinct(id)
    }
  }
  return(ids)
}

build_tag_id_data <- function(tags) {
  tag_ids <- dplyr::tibble() %>% tibble::add_column(id = NA)
  for (row in 1:nrow(tags)) {
    tag_ids <- tags %>% get_ids_from_heirarchy(tag_ids, tags[row, "id"])
  }
  return(tag_ids)
}

rebuild_samples_to_tags <- function(samples_to_tags, id, current_sample_id, sample_set, tags) {
  purrr::pmap(sample_set, ~{
    current_sample <- ..1
    current_tcga_study <- ..2
    current_tcga_subtype <- ..3
    current_immune_subtype <- ..4
    if (current_tcga_study %in% tags$name) {
      tag_row <- tags %>% dplyr::filter(name == current_tcga_study)
      samples_to_tags <<- samples_to_tags %>%
        dplyr::add_row(sample_id = id, tag_id = tag_row[["id"]])
    }
  })
  samples_to_tags <- samples_to_tags %>% dplyr::distinct(sample_id, tag_id)
  return(samples_to_tags)
}
