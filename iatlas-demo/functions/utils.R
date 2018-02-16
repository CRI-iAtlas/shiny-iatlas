

create_membership_list <- function(){
    names <- feature_table %>% 
        filter(!is.na(`Variable Class`)) %>% 
        use_series(`Variable Class`) %>% 
        unique
    map(names, get_variable_group) %>% 
        set_names(names)
}

get_variable_group <- function(name){
    df <- feature_table %>% 
        select(`Variable Class`, FeatureMatrixLabelTSV, `Variable Class Order`) %>% 
        filter(`Variable Class` == name) %>% 
        .[complete.cases(.),] %>% 
        arrange(`Variable Class Order`)
    factor(df$FeatureMatrixLabelTSV, levels = df$FeatureMatrixLabelTSV)
}

# formatting

get_display_name <- function(name){
    feature_table %>% 
        filter(FeatureMatrixLabelTSV == name) %>% 
        use_series(FriendlyLabel)
}



buildDataFrame_corr <- function(dat, var1, var2, catx) {
    getCats <- function(dat, catx) {
        cats <- as.character(na.omit(unique(dat[,catx])))
    }
    
    # get the vectors associated with each term
    cats <- sort(getCats(dat, catx))
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
    rownames(cormat) <- sapply(rownames(cormat), get_display_name)
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

get_label_from_data_obj <- function(obj, obj_list, selection){
    obj %>% 
        magrittr::extract2(obj_list) %>% 
        magrittr::extract2(selection) %>% 
        as.character
}

decide_plot_colors <- function(data_obj, sample_group_label){
    color_mapping <- c(
        'Study' = "tcga_colors",
        'Subtype_Immune_Model_Based' = "subtype_colors")
    if(!sample_group_label %in% names(color_mapping)) return(NA)
    color_item  <- magrittr::extract2(color_mapping, sample_group_label)
    plot_colors <- magrittr::extract2(data_obj, color_item)
}
