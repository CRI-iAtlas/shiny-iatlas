## Transform functions are used globally and within individual modules to
## subset, arrange, or otherwise modify data prior to visualization with tables
## or plots

# Global data transform ----

# ** PanImmune data subsetting ----

subset_panimmune_df <- function(
    df = panimmune_data$df, group_col, study_subtype
) {
    if (!(group_col == "Subtype_Curated_Malta_Noushmehr_et_al")) {
        return(df)
    } else {
        let(
            alias = c(COL = group_col), {
                sample_groups <- df %>% 
                    select(COL) %>% 
                    distinct() %>% 
                    mutate(study = str_extract(COL, ".*(?=\\.)"),
                           study = str_split(study, "_")) %>% 
                    unnest(study) %>% 
                    filter(study %in% study_subtype) %>% 
                    extract2(group_col)
                
                panimmune_data$df %>% 
                    filter(COL %in% sample_groups) %>% 
                    mutate(COL = fct_drop(COL))
            }
        )
    }
}

# Module specific data transform ----

# ** Sample groups overview module ----

create_intermediate_corr_df <- function(
    subset_df, dep_var, facet_selection, facet_groups, indep_vars
) {
    subset_df %>%
        as_data_frame() %>%
        filter(UQ(as.name(facet_selection)) %in% facet_groups) %>%
        select_(.dots = c(facet_selection, dep_var, indep_vars))
}


create_heatmap_corr_mat <- function(
    df, dep_var, facet_selection, facet_groups, indep_vars
) {
    
    get_correlation <- function(var1, var2, df) {
        cor(select_(df, var1),
            select_(df, var2),
            method = "spearman",
            use = "pairwise.complete.obs"
        )
    }
    
    facet_groups <- facet_groups[facet_groups %in% extract2(df, facet_selection)]
    cormat <- matrix(
        data = 0,
        ncol = length(facet_groups),
        nrow = length(indep_vars)
    )
    rownames(cormat) <- indep_vars
    colnames(cormat) <- facet_groups
    # for each factor in facet_groups
    for (ci in facet_groups) {
        # subset df
        subdat <- df[df[, facet_selection] == ci, ]
        # compute correlation
        for (var in indep_vars) {
            cormat[var, ci] <- get_correlation(var, dep_var, subdat)
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
    return(plot_df)
}

# ** Tumor composition module ----

create_tumor_content_df <- function(subset_df, sampgroup, cellcontent) {
    subset_df %>%
        select(sampgroup, cellcontent) %>%
        .[complete.cases(.), ]
}

create_cell_fraction_df <- function(subset_df, group_column) {
    let(
        alias = c(group_col = group_column),
        subset_df %>% 
            select(group_col, Stromal_Fraction, leukocyte_fraction) %>% 
            .[complete.cases(.),] %>% 
            mutate(Tumor_Fraction = 1 - Stromal_Fraction) %>% 
            gather(fraction_name, fraction, -group_col) %>% 
            mutate(fraction_name = str_replace(fraction_name, "Stromal_Fraction", "Stromal Fraction")) %>% 
            mutate(fraction_name = str_replace(fraction_name, "leukocyte_fraction", "Leukocyte Fraction")) %>% 
            mutate(fraction_name = str_replace(fraction_name, "Tumor_Fraction", "Tumor Fraction")) %>% 
            group_by(group_col, fraction_name) %>% 
            summarise(mean_fraction = mean(fraction), sd_fraction = sd(fraction)) %>% 
            mutate(group = str_c(fraction_name, group_col, sep = ":"))
    )
}

# ** Clinical outcomes module ----

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

# ** Immune interface module ----

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

# ** Immunomodulators module ----

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






