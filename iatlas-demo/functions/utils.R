get_variable_group <- function(name) {
  df <- feature_table %>%
    select(`Variable Class`, FeatureMatrixLabelTSV, `Variable Class Order`) %>%
    filter(`Variable Class` == name) %>%
    .[complete.cases(.), ] %>%
    arrange(`Variable Class Order`)
  factor(df$FeatureMatrixLabelTSV, levels = df$FeatureMatrixLabelTSV)
}

get_category_group <- function(category) {
  panimmune_data$df %>%
    extract2(category) %>%
    na.omit() %>%
    unique() %>%
    sort() %>%
    as.character()
}

# these switch between internal name and display name
switch_names <- function(df, name, old_col, new_col) {
  df %>%
    filter(.data[[old_col]] == name) %>%
    extract2(new_col)
}

get_variable_display_name <- function(name) {
  switch_names(
    feature_table,
    name,
    "FeatureMatrixLabelTSV",
    "FriendlyLabel"
  )
}

get_variable_internal_name <- function(name) {
  switch_names(
    feature_table,
    name,
    "FriendlyLabel",
    "FeatureMatrixLabelTSV"
  )
}

get_modulator_display_name <- function(name) {
  switch_names(
    panimmune_data$direct_relationship_modulators,
    name,
    "HGNC_Symbol",
    "Gene"
  )
}

get_modulator_internal_name <- function(name) {
  switch_names(
    panimmune_data$direct_relationship_modulators,
    name,
    "Gene",
    "HGNC_Symbol"
  )
}

decide_plot_colors <- function(data_obj, sample_group_label) {
  color_mapping <- c(
    "Study" = "tcga_colors",
    "Subtype_Immune_Model_Based" = "subtype_colors"
  )
  if (!sample_group_label %in% names(color_mapping)) return(NA)
  color_item <- magrittr::extract2(color_mapping, sample_group_label)
  plot_colors <- magrittr::extract2(data_obj, color_item)
}

