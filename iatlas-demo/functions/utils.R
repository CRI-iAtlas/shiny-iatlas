get_variable_group <- function(name){
    df <- feature_table %>%
        select(`Variable Class`, FeatureMatrixLabelTSV, `Variable Class Order`) %>%
        filter(`Variable Class` == name) %>%
        .[complete.cases(.),] %>%
        arrange(`Variable Class Order`)
    factor(df$FeatureMatrixLabelTSV, levels = df$FeatureMatrixLabelTSV)
}

get_category_group <- function(category){
    panimmune_data$df %>% 
        extract2(category) %>% 
        na.omit %>% 
        unique %>% 
        sort %>% 
        as.character
}

# these switch between internal name and display name
switch_names <- function(df, name, old_col, new_col){
    df %>%
        filter(.data[[old_col]] == name) %>% 
        extract2(new_col)
}
   
get_variable_display_name <- function(name){
    switch_names(feature_table, 
                 name, 
                 "FeatureMatrixLabelTSV", 
                 "FriendlyLabel")
}

get_variable_internal_name <- function(name){
    switch_names(feature_table, 
                 name, 
                 "FriendlyLabel", 
                 "FeatureMatrixLabelTSV")
}

get_modulator_display_name <- function(name){
    switch_names(panimmune_data$direct_relationship_modulators, 
                 name, 
                 "HGNC_Symbol", 
                 "Gene")
}

get_modulator_internal_name <- function(name){
    switch_names(panimmune_data$direct_relationship_modulators,
                 name, 
                 "Gene", 
                 "HGNC_Symbol")
}




decide_plot_colors <- function(data_obj, sample_group_label){
    color_mapping <- c(
        'Study' = "tcga_colors",
        'Subtype_Immune_Model_Based' = "subtype_colors")
    if(!sample_group_label %in% names(color_mapping)) return(NA)
    color_item  <- magrittr::extract2(color_mapping, sample_group_label)
    plot_colors <- magrittr::extract2(data_obj, color_item)
}



buildDataFrame_corr <- function(dat, var1, var2, catx) {
    
    # get the vectors associated with each term
    cats <- get_category_group(catx)
    vars <- get_variable_group(var1)
    
    # this would be the correlations
    cormat <- matrix(data=0, ncol=length(cats), nrow=length(vars))
    rownames(cormat) <- vars
    colnames(cormat) <- cats
    # for each factor in catx
    for (ci in cats) {
        # subset dat
        subdat <- dat[dat[,catx] == ci,]
        # compute correlation 
        for (vi in vars) {
            cormat[vi,ci] <- cor(subdat[,vi], subdat[,var2], method="spearman", 
                                 use="pairwise.complete.obs")
        }      
    }  
    # give it nice names
    rownames(cormat) <- sapply(rownames(cormat), get_variable_display_name)
    cormat[is.na(cormat)] <- 0
    cormat
}

buildDataFrame_surv <- function(dat, var1, timevar, divk) {
    getCats <- function(dat, var1, divk) {
        
        if (var1 %in% c("Subtype_Immune_Model_Based")) {
            # then we don't need to produce catagories.
            cats <- as.character(dat[,var1])
        }
        else {
            cats <- as.character(cut(dat[,var1], divk, ordered_result=T))
        }
        cats
    }
    
    # get the vectors associated with each term
    # if var1 is already a catagory, just use that.
    # otherwise it needs to be divided into divk catagories.
    cats <- getCats(dat, var1, divk)
    
    if (timevar == "OS_time") {
        timevar2 <- "OS"
    } else {
        timevar2 <- "PFI_1"
    }
    
    df <- data.frame(Status = dat[,timevar2], Time = dat[,timevar], 
                     Variable=cats, Measure=dat[,var1])
    df <- na.omit(df)
    df
}

# immunomodulator helpers -----------------------------------------------------

create_im_gene_boxplot_df <- function(im_choice, ss_group){
    panimmune_data$immunomodulator_df %>% 
        filter(Symbol == im_choice) %>% 
        left_join(panimmune_data$df) %>% 
        mutate(log_count = log10(normalized_count + 1)) %>% 
        select(ss_group, log_count) %>% 
        .[complete.cases(.), ] 
}

create_im_gene_histplot_df <- function(
    boxplot_df, boxplot_column, boxplot_selected_group){
    
    filter(boxplot_df, UQ(as.name(boxplot_column)) == boxplot_selected_group)
}

get_selected_group_from_plotly_boxplot <- function(
    plot_df, plot_column, eventdata){
    
    selected_box_index <- eventdata$x[[1]]
    plot_df %>% 
        extract2(plot_column) %>% 
        as.factor %>% 
        levels %>% 
        extract2(selected_box_index)
}

# feature correlation helpers -------------------------------------------------

create_feature_correlation_df <- function(dat, var1, var2, catx) {
    
    dat  <- panimmune_data$df
    var1 <- "Immune Cell Proportion - Aggregate 2"
    var2 <- "leukocyte_fraction"
    catx <- "Subtype_Immune_Model_Based"
    
    cats <- get_category_group(catx)
    vars <- get_variable_group(var1)
    
    # this would be the correlations
    all_cats_df <- dat %>% 
        as_data_frame %>% 
        filter(UQ(as.name(catx)) %in% cats) %>% 
        select_(.dots = c(catx, var2, as.character(vars)))

    cormat2 <- create_correlation_matrix(all_cats_df, cats, vars, var2)
}

create_correlation_matrix <- function(dat, cats, vars, var2){
    cormat <- matrix(
        data = 0, 
        ncol = length(cats),
        nrow = length(vars))
    rownames(cormat) <- vars
    colnames(cormat) <- cats
    # for each factor in catx
    for (ci in cats) {
        # subset dat
        subdat <- dat[dat[,catx] == ci,]
        # compute correlation 
        for (var in vars) {
            cormat[var,ci] <- get_correlation(var, var2, subdat)
        }      
    }  
    # give it nice names
    rownames(cormat) <- sapply(rownames(cormat), get_variable_display_name)
    cormat[is.na(cormat)] <- 0
    return(cormat)
}

get_correlation <- function(var1, var2, df){
    cor(select_(df, var1),
        select_(df, var2),
        method = "spearman", 
        use    = "pairwise.complete.obs")
}


get_scatterplot_df <- function(dat, var1, var2, catx){
    # dat  <- panimmune_data$df
    # var1 <- "Immune Cell Proportion - Aggregate 2"
    # var2 <- "leukocyte_fraction"
    # catx <- "Subtype_Immune_Model_Based"
    # get the vectors associated with each term
    cats <- get_category_group(catx)
    vars <- get_variable_group(var1)
    df2 <- dat %>% 
        as_data_frame %>% 
        filter(UQ(as.name(catx)) %in% cats) %>% 
        select_(.dots = c(catx, var2, as.character(vars)))
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
