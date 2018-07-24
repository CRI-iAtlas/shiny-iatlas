
# these switch between internal name and display name -------------------------

get_group_internal_name <- function(input_name, df = panimmune_data$feature_df){
    internal_name <- get_variable_internal_name(input_name, df)
    if (length(internal_name) == 0){
        return(input_name) # user supplied group names
    } else if (length(internal_name) == 1) {
        return(internal_name) # standard group names
    } else {
        stop("group name has multiple matches: ", 
             input_name, 
             " matches: ", 
             str_c(internal_name, collapse = ", "))
    }
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
    
    wrapr::let(
        alias = c(OLD_COL = old_col), 
        expr  = {
            df %>%
                dplyr::filter(OLD_COL == value) %>%
                magrittr::extract2(new_col)
            }
    )
}

# factor variables ------------------------------------------------------------

get_factored_variables_from_feature_df <- function(class_name){
    get_factored_variables_by_class(
        class_name, 
        df = panimmune_data$feature_df,
        class_column = "Variable_Class",
        variable_column = "FeatureMatrixLabelTSV",
        order_column = "Variable_Class_Order" 
    )
}

get_factored_variables_by_class <- function(
    class_name, df, class_column, variable_column, order_column){
    
    class_df <- get_complete_class_df(
        class_name, df, class_column, variable_column, order_column)
    if (nrow(class_df) == 0) {
        stop("empty class: ", class_name)
    }
    factor_variables_with_df(class_df, variable_column, order_column)
}

factor_variables_with_df <- function(df, variable_column, order_column){
    result <- 
        wrapr::let( 
            alias = c(ORDER = order_column),
            expr = dplyr::arrange(df, ORDER)) %>% 
        magrittr::extract2(variable_column) %>% 
        factor(., levels = .)
}

get_complete_class_df <- function(
    class_name, df, class_column, variable_column, order_column){
    

    temp_df <- df %>% 
        magrittr::set_colnames(stringr::str_replace_all(
            colnames(.), 
            " ", 
            "_")) %>% 
        get_complete_df_by_columns(c(
            class_column, 
            variable_column, 
            order_column)) 
    result_df <- 
        wrapr::let( 
            alias = c(CLASS = class_column),
            expr = dplyr::filter(temp_df, CLASS == class_name)) %>% 
        dplyr::select(variable_column, order_column)

}


get_complete_df_by_columns <- function(df, columns){
    df %>%
        dplyr::select(columns) %>%
        .[complete.cases(.),] 
}

# -----------------------------------------------------------------------------

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

## selection choices for the dropdown menu of sample groups
# create_sample_group_options <- function(feature_df) {
#     feature_df %>%
#         filter(`Variable Class` == "Sample Category") %>%
#         use_series(FeatureMatrixLabelTSV) 
# }
