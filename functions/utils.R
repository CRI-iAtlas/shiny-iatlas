
# these switch between internal name and display name -------------------------

get_group_internal_name <- function(input_name){
    internal_name <- get_variable_internal_name(input_name)
    len <- length(internal_name)
    result <- dplyr::case_when(
        len == 0 ~ input_name, # user supplied group names
        len == 1 ~ internal_name, # standard group names
        len > 1 ~ stop("group name has multiple matches: ", input_name) 
    )
    return(result)
}

get_variable_display_name <- function(name, df = panimmune_data$feature_df){
    convert_value_between_columns(
        df, name, "FeatureMatrixLabelTSV", "FriendlyLabel")
}

get_variable_internal_name <- function(name, df = panimmune_data$feature_df){
    convert_value_between_columns(
        df, name, "FriendlyLabel", "FeatureMatrixLabelTSV")
}

get_im_display_name <- function(
    name, df = panimmune_data$im_direct_relationships){
    
    convert_value_between_columns(df, name, "HGNC Symbol", "Gene")
}

convert_value_between_columns <- function(
    df, value, old_col, new_col, return_one_value = F) {
    
    new_values <- wrapr::let(
        alias = c(OLD_COL = old_col), 
        expr  = {
            df %>%
                dplyr::filter(OLD_COL == value) %>%
                magrittr::extract2(new_col)
            }
    )
    if(length(new_values) == 0 && return_one_value){
        stop("value has no match in new column: ", value)
    } else if(length(new_values) > 1 && return_one_value){
        stop("value has multiple matches in new column: ", value)
    }
    return(new_values)
}

# -----------------------------------------------------------------------------

set_names_to_self <- function(lst) {
    if (length(lst) == 0){ 
        stop("imput list/vector empty")
    }
    magrittr::set_names(lst, lst)
}

get_variable_group <- function(name, df) {
    filtered_df <- df %>%
        dplyr::select(
            `Variable Class`, 
            FeatureMatrixLabelTSV, 
            `Variable Class Order`) %>%
        dplyr::filter(`Variable Class` == name) %>% 
        .[complete.cases(.),] 
    if (nrow(filtered_df) == 0) {
        stop("group empty")
    }
    ordered_labels <- filtered_df %>% 
        dplyr::arrange(`Variable Class Order`) %>% 
        magrittr::use_series(FeatureMatrixLabelTSV)
    factor(ordered_labels, levels = ordered_labels)
}


get_unique_column_values <- function(category, df){
    df %>% 
        magrittr::extract2(category) %>% 
        na.omit() %>%
        unique() %>%
        sort() %>%
        as.character()
}



decide_plot_colors <- function(data_obj, sample_group_label, subset_df = NULL) {
    color_mapping <- c(
        "Study" = "tcga_study_colors",
        "Subtype_Immune_Model_Based" = "immune_subtype_colors",
        "Subtype_Curated_Malta_Noushmehr_et_al" = "tcga_subtype_colors"
    )
    if (sample_group_label %in% names(color_mapping)) {
        color_item <- magrittr::extract2(color_mapping, sample_group_label)
        magrittr::extract2(data_obj, color_item)
    } else {
        groups <- subset_df %>% 
            magrittr::extract2(sample_group_label) %>% 
            unique %>% 
            sort
        colors <- RColorBrewer::brewer.pal(length(groups), "Set1")
        set_names(colors, groups)
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


check_click_data <- function(eventdata, subset_df, group_internal_choice, intermediate_corr_df){
    if(is.null(eventdata)) return(FALSE)
    all(eventdata$x[[1]] %in% extract2(subset_df, group_internal_choice),
        any(get_variable_internal_name(eventdata$y[[1]]) %in% colnames(intermediate_corr_df)))
}

