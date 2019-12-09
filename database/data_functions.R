is_df_empty <- function(df) {
  return(is.null(dim(df)) | dim(df)[1] == 0 | dim(df)[2] == 0)
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

update_samples_to_tags <- function(samples_to_tags, id, tags) {
  # Ensure data.frames.
  tags <- tags %>% as.data.frame
  if (!is_df_empty(tags)) {
    tag_ids <- tags %>% 
      build_tag_id_data %>% 
      dplyr::distinct(id) %>%
      as.data.frame
    for (row in 1:nrow(tag_ids)) {
      samples_to_tags <- samples_to_tags %>%
        dplyr::select(dplyr::everything()) %>%
        dplyr::rowwise() %>%
        dplyr::add_row(sample_id = id, tag_id = tag_ids[row, "id"])
    }
  }
  return(samples_to_tags)
}

rebuild_samples_to_tags <- function(samples_to_tags, id, current_sample_id, all_samples, tags) {
  current_sample_set <- all_samples %>%
    dplyr::select(sample:Immune_Subtype) %>%
    dplyr::filter(sample == current_sample_id) %>% 
    as.data.frame
  if (!is_df_empty(current_sample_set)) {
    for (row in 1:nrow(current_sample_set)) {
      current_tags <- tags %>% 
        dplyr::select(id, name, parent) %>% 
        dplyr::filter(name == current_sample_set[row, "TCGA_Study"] |
                        name == current_sample_set[row, "TCGA_Subtype"] |
                        name == current_sample_set[row, "Immune_Subtype"])
      samples_to_tags <- samples_to_tags %>% update_samples_to_tags(id, current_tags)
    }
  }
  samples_to_tags <- samples_to_tags %>% dplyr::distinct(sample_id, tag_id)
  return(samples_to_tags)
}

rebuild_gene_relational_data <- function(all_genes, gene_row, field_name = "name", relational_data = dplyr::tibble()) {
  if (is_df_empty(relational_data)) {
    relational_data <- dplyr::tibble() %>%
      tibble::add_column(!!field_name := NA)
  }
  relational_data <- relational_data %>%
    dplyr::select(dplyr::everything()) %>%
    dplyr::rowwise() %>%
    dplyr::add_row(!!field_name := all_genes[row, gene_row])
  return(relational_data)
}
