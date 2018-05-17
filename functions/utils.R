set_names_to_self <- function(lst) {
  set_names(lst, lst)
}

get_variable_group <- function(name, df = NULL) {
  if (is.null(df)) {
    df <- panimmune_data$feature_df
  }
  df <- df %>%
    select(`Variable Class`, FeatureMatrixLabelTSV, `Variable Class Order`) %>%
    filter(`Variable Class` == name) %>%
    .[complete.cases(.), ] %>%
    arrange(`Variable Class Order`)
  factor(df$FeatureMatrixLabelTSV, levels = df$FeatureMatrixLabelTSV)
}

get_category_group <- function(category) {
  panimmune_data$fmx_df %>%
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

get_variable_display_name <- function(name, df = NULL) {
  if (is.null(df)) {
    df <- panimmune_data$feature_df
  }
  switch_names(
    df,
    name,
    "FeatureMatrixLabelTSV",
    "FriendlyLabel"
  )
}

get_variable_internal_name <- function(name, df = NULL) {
  if (is.null(df)) {
    df <- panimmune_data$feature_df
  }
  switch_names(
    df,
    name,
    "FriendlyLabel",
    "FeatureMatrixLabelTSV"
  )
}

get_im_display_name <- function(name, df = NULL) {
  if (is.null(df)) {
    df <- panimmune_data$im_direct_relationships
  }
  switch_names(
    df,
    name,
    "HGNC Symbol",
    "Gene"
  )
}

get_im_internal_name <- function(name) {
  switch_names(
    panimmune_data$direct_relationship_modulators,
    name,
    "Gene",
    "HGNC Symbol"
  )
}

decide_plot_colors <- function(data_obj, sample_group_label) {
  color_mapping <- c(
    "Study" = "tcga_study_colors",
    "Subtype_Immune_Model_Based" = "immune_subtype_colors",
    "Subtype_Curated_Malta_Noushmehr_et_al" = "tcga_subtype_colors"
  )
  if (sample_group_label %in% names(color_mapping)) {
      color_item <- magrittr::extract2(color_mapping, sample_group_label)
      magrittr::extract2(data_obj, color_item)
  } else {
      return(NA)
  }
}

get_friendly_numeric_columns <- function(){
    get_numeric_columns() %>% 
        purrr::map(get_variable_display_name) %>%
        compact() %>% 
        unlist() %>% 
        discard(~is.na(.))
}

get_friendly_numeric_columns_by_group <- function() {
  panimmune_data$feature_df %>% 
    select(Class = `Variable Class`, FriendlyLabel, FeatureMatrixLabelTSV) %>% 
    filter(FriendlyLabel %in% get_friendly_numeric_columns()) %>% 
    mutate(Class = ifelse(is.na(Class), "Other", Class)) %>% 
    nest(-Class) %>% 
    mutate(data = map(data, deframe)) %>% 
    deframe()
}

get_numeric_columns <- function(){
    panimmune_data$fmx_df %>% 
        select_if(is.numeric) %>% 
        colnames()
}

get_numeric_variable_classes <- function(){
    panimmune_data %>% 
        extract2("feature_df") %>% 
        filter(`VariableType` == "Numeric") %>% 
        extract2("Variable Class") %>% 
        unique %>% 
        discard(is.na(.))
}

