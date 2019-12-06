is_df_empty <- function(df) {
  return(is.null(dim(df)) | dim(df)[1] == 0 | dim(df)[2] == 0)
}

value_to_id <- function(value, int_value, id_value, name_value) {
  if (identical(value, name_value)) {
    return(id_value)
  } else if (!is.na(int_value)) {
    return(int_value)
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

rebuild_groups <- function(groups, all_groups) {
  # Ensure data.frames.
  all_groups <- all_groups %>%
    dplyr::select(id:name) %>%
    as.data.frame
  for (row in 1:nrow(all_groups)) {
    groups <- groups %>%
      dplyr::select(dplyr::everything()) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(parent = value_to_id(parent_group, parent, all_groups[row, "id"], all_groups[row, "name"])) %>%
      dplyr::mutate(subgroup = value_to_id(subtype_group, subgroup, all_groups[row, "id"], all_groups[row, "name"]))
  }
  return(groups)
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

build_group_id_data <- function(groups) {
  group_ids <- dplyr::tibble() %>% tibble::add_column(id = NA)
  for (row in 1:nrow(groups)) {
    group_ids <- groups %>% get_ids_from_heirarchy(group_ids, groups[row, "id"])
  }
  return(group_ids)
}

update_samples_to_groups <- function(samples_to_groups, id, groups) {
  # Ensure data.frames.
  groups <- groups %>% as.data.frame
  if (!is_df_empty(groups)) {
    group_ids <- groups %>% 
      build_group_id_data %>% 
      dplyr::distinct(id) %>%
      as.data.frame
    for (row in 1:nrow(group_ids)) {
      samples_to_groups <- samples_to_groups %>%
        dplyr::select(dplyr::everything()) %>%
        dplyr::rowwise() %>%
        dplyr::add_row(sample_id = id, group_id = group_ids[row, "id"])
    }
  }
  return(samples_to_groups)
}

rebuild_samples_to_groups <- function(samples_to_groups, id, current_sample_id, all_samples, groups) {
  current_sample_set <- all_samples %>%
    dplyr::select(sample:Immune_Subtype) %>%
    dplyr::filter(sample == current_sample_id) %>% 
    as.data.frame
  if (!is_df_empty(current_sample_set)) {
    for (row in 1:nrow(current_sample_set)) {
      current_groups <- groups %>% 
        dplyr::select(id, name, parent) %>% 
        dplyr::filter(name == current_sample_set[row, "TCGA_Study"] |
                        name == current_sample_set[row, "TCGA_Subtype"] |
                        name == current_sample_set[row, "Immune_Subtype"])
      samples_to_groups <- samples_to_groups %>% update_samples_to_groups(id, current_groups)
    }
  }
  samples_to_groups <- samples_to_groups %>% dplyr::distinct(sample_id, group_id)
  return(samples_to_groups)
}

rebuild_gene_relational_data <- function(all_genes, gene_row, field_name = "name", relational_data = dplyr::tibble()) {
  if (is_df_empty(relational_data)) {
    relational_data <- dplyr::tibble() %>%
      tibble::add_column(!!field_name := NA)
  }
  relational_data <- relational_data %>%
    dplyr::select(everything()) %>%
    dplyr::rowwise() %>%
    dplyr::add_row(!!field_name := all_genes[row, gene_row])
  return(relational_data)
}
