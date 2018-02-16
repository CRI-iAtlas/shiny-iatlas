# look up tables to get sets of column names in the feature matrix.

getVars <- function(var1) {
    config_yaml %>% 
        extract2(var1) %>% 
        unlist
}

# formatting

getNiceName <- function(x) {
    config_yaml$nice_names[x]
}

buildDataFrame_corr <- function(dat, var1, var2, catx) {
    getCats <- function(dat, catx) {
        cats <- as.character(na.omit(unique(dat[,catx])))
    }
    
    # get the vectors associated with each term
    cats <- sort(getCats(dat, catx))
    vars <- sort(getVars(var1))
    
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
    #colnames(cormat) <- sapply(colnames(cormat), getNiceName)
    rownames(cormat) <- sapply(rownames(cormat), getNiceName)
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
