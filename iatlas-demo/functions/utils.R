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


# immunomodulator helpers -----------------------------------------------------

create_im_gene_histplot_df <- function(
                                       boxplot_df, boxplot_column, boxplot_selected_group) {
  filter(boxplot_df, UQ(as.name(boxplot_column)) == boxplot_selected_group)
}

get_selected_group_from_plotly_boxplot <- function(
                                                   plot_df, plot_column, eventdata) {
  selected_box_index <- eventdata$x[[1]]
  plot_df %>%
    extract2(plot_column) %>%
    as.factor() %>%
    levels() %>%
    extract2(selected_box_index)
}



# unused functions ------------------------------------------------------------

# get_label_from_data_obj <- function(obj, obj_list, selection){
#     obj %>%
#         magrittr::extract2(obj_list) %>%
#         magrittr::extract2(selection) %>%
#         as.character
# }
#
#
# reverse_named_list <- function(lst){
#     new_names <- unname(lst)
#     new_items <- names(lst)
#     new_list <- set_names(new_items, new_names)
# }

# create_membership_list <- function(){
#     names <- panimmune_data$feature_table %>%
#         filter(!is.na(`Variable Class`)) %>%
#         use_series(`Variable Class`) %>%
#         unique
#     map(names, get_variable_group) %>%
#         set_names(names)
# }


#
# get_modulators_group <- function(name){
#     df <- panimmune_data$direct_relationship_modulators %>%
#         select(`Variable Class`, FeatureMatrixLabelTSV, `Variable Class Order`) %>%
#         filter(`Variable Class` == name) %>%
#         .[complete.cases(.),] %>%
#         arrange(`Variable Class Order`)
#     factor(df$FeatureMatrixLabelTSV, levels = df$FeatureMatrixLabelTSV)
# }
