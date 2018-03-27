# panimmune df subset ---------------------------------------------------------

subset_panimmune_df <- function(group_col, study_subtype) {
    if (!(group_col == "Subtype_Curated_Malta_Noushmehr_et_al")) {
        return(panimmune_data$df)
    } else {
        let(
            alias = c(COL = group_col), {
                sample_groups <- panimmune_data$df %>% 
                    select(COL) %>% 
                    distinct() %>% 
                    mutate(study = str_extract(COL, ".*(?=\\.)")) %>% 
                    mutate(study = str_split(study, "_")) %>% 
                    unnest(study) %>% 
                    filter(study %in% study_subtype) %>% 
                    extract2(group_col)
                
                panimmune_data$df %>% 
                    filter(COL %in% sample_groups)
            }
        )
    }
}

# tumor content ---------------------------------------------------------------

create_tumor_content_df <- function(subset_df, sampgroup, cellcontent) {
    subset_df %>%
        select(sampgroup, cellcontent) %>%
        .[complete.cases(.), ]
}

# immune interface ------------------------------------------------------------

create_immuneinterface_df <- function(subset_df, sample_group, diversity_vars) {
    plot_df <- subset_df %>%
        select(sample_group, diversity_vars) %>%
        .[complete.cases(.), ] %>%
        gather(metric, diversity, -1) %>%
        separate(metric, into = c("receptor", "metric"), sep = "_")
}

ztransform_df <- function(df) {
    df %>%
        group_by(receptor, metric) %>%
        mutate(
            div_mean = mean(diversity),
            div_sd = sd(diversity)
        ) %>%
        ungroup() %>%
        mutate(diversity = (diversity - div_mean) / div_sd)
}

# sample groups ---------------------------------------------------------------

create_intermediate_corr_df <- function(subset_df, var2, catx, cats, vars) {
    subset_df %>%
        as_data_frame() %>%
        filter(UQ(as.name(catx)) %in% cats) %>%
        select_(.dots = c(catx, var2, vars))
}


create_heatmap_corr_matrix <- function(dat, var2, catx, cats, vars) {
    
    get_correlation <- function(var1, var2, df) {
        cor(select_(df, var1),
            select_(df, var2),
            method = "spearman",
            use = "pairwise.complete.obs"
        )
    }
    
    cats <- cats[cats %in% extract2(dat, catx)]
    cormat <- matrix(
        data = 0,
        ncol = length(cats),
        nrow = length(vars)
    )
    rownames(cormat) <- vars
    colnames(cormat) <- cats
    # for each factor in catx
    for (ci in cats) {
        # subset dat
        subdat <- dat[dat[, catx] == ci, ]
        # compute correlation
        for (var in vars) {
            cormat[var, ci] <- get_correlation(var, var2, subdat)
        }
    }
    # give it nice names
    rownames(cormat) <- sapply(rownames(cormat), get_variable_display_name)
    cormat[is.na(cormat)] <- 0
    return(cormat)
}

create_scatterplot_df <- function(
    df, category_column, category_plot_selection, internal_variable_name,
    variable2_selection ) {
    
    plot_df <- df %>%
        filter(UQ(as.name(category_column)) == category_plot_selection) %>%
        select_(.dots = variable2_selection, internal_variable_name)
    print(plot_df)
    return(plot_df)
}

# immunomodulators ------------------------------------------------------------

build_boxplot_df <- function(subset_df, im_choice, ss_group) {
    panimmune_data$immunomodulator_df %>%
        filter(Symbol == im_choice) %>%
        left_join(subset_df) %>%
        mutate(log_count = log10(normalized_count + 1)) %>%
        select(ss_group, log_count) %>%
        .[complete.cases(.), ]
}

build_histogram_df <- function(
    boxplot_df, boxplot_column, boxplot_selected_group) {

    plot_df <- boxplot_df %>% 
        filter(UQ(as.name(boxplot_column)) == boxplot_selected_group)

}

# dataframe builders



build_survival_df <- function(dat, var1, timevar, divk) {
  getCats <- function(dat, var1, divk) {
    if (var1 %in% c("Subtype_Immune_Model_Based")) {
      # then we don't need to produce catagories.
      cats <- as.character(dat[, var1])
    }
    else {
      cats <- as.character(cut(dat[, var1], divk, ordered_result = T))
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
  
  df <- data.frame(
    Status = dat[, timevar2], Time = dat[, timevar],
    Variable = cats, Measure = dat[, var1]
  )
  df <- na.omit(df)
  df
}




