###############################################################################
# These functions have been refactored and have unit tests.
# Do not make any modifications to these!
# If you want to make a modification, please copy and paste the function the
# lower section and call it <function_name>2.
# Make any needed modifcations to the coipied function.
# The new functionality will get unut tests and be folded back into the 
# original function.
###############################################################################


###############################################################################
# Tests below this line do not have tests yet, newly writen functions 
###############################################################################


check_immunefeatures_scatterplot_click_data <- function(
    eventdata, subset_df, group_column, corr_df){
    
    
    if(is.null(eventdata)) {
        return(FALSE)  
    } 
    column_name <- eventdata$x[[1]]
    row_name  <- eventdata$y[[1]]
    column_name_valid <- column_name %in% extract2(subset_df, group_column)
    row_name_valid <- any(
        get_variable_internal_names(row_name) %in% colnames(corr_df))
    all(column_name_valid, row_name_valid)
}

check_driver_violinplot_click_data <- function(
    eventdata, df){
    
    if(is.null(eventdata)) {
        return(FALSE)  
    } 
    group_selected <- eventdata[["key"]][[1]][1]
    group_valid <- group_selected %in% extract2(df, "mutation_group")
    
    all(group_valid)
}

create_group_text_from_plotly <- function(
    source_name, group_choice, sample_group_df,
    source_type = "plotly_click",
    prompt_text = "Click above plot for more group information.",
    key_column = "key"){
    
    data <- event_data(source_type, source = source_name)
    
    if (is.null(data)) return(prompt_text)
   
    key_value <- data %>%
        slice(1) %>% 
        extract2(key_column)
    
    text <- sample_group_df %>% 
        dplyr::filter(FeatureValue == key_value) %>%  
        distinct() %>% 
        slice(1) %>% 
        mutate(Characteristics = 
                   ifelse(is.na(Characteristics), 
                          "No additional information.", 
                          Characteristics)) %>% 
        mutate(name = 
                   ifelse(is.na(FeatureName), 
                          FeatureValue, 
                          FeatureName)) %>% 
        mutate(text = str_c(name, ": ", Characteristics)) %>% 
        use_series(text)
}