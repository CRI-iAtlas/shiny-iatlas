# panimmune df subset ---------------------------------------------------------

subset_panimmune_df <- function(study_col, study_subtypes){
    let(
        alias = c(COL = study_col),
        panimmune_data$df %>%
            filter(COL %in% study_subtypes))
}

# tumor content df ------------------------------------------------------------

create_tumor_content_df <- function(subset_df, sampgroup, cellcontent) {
    subset_df %>%
        select(sampgroup, cellcontent) %>%
        .[complete.cases(.), ]
}

# dataframe builders

build_scatterplot_df <- function(
  df, category_column, category_plot_selection, internal_variable_name,
  variable2_selection
) {
  plot_df <- df %>%
    filter(UQ(as.name(category_column)) == category_plot_selection) %>%
    select_(.dots = variable2_selection, internal_variable_name)
}

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

build_histogram_df <- function(
  boxplot_df, boxplot_column, boxplot_selected_group
) {
  filter(boxplot_df, UQ(as.name(boxplot_column)) == boxplot_selected_group)
}

build_boxplot_df <- function(im_choice, ss_group) {
  panimmune_data$immunomodulator_df %>%
    filter(Symbol == im_choice) %>%
    left_join(panimmune_data$df) %>%
    mutate(log_count = log10(normalized_count + 1)) %>%
    select(ss_group, log_count) %>%
    .[complete.cases(.), ]
}

# matrix builders

build_correlation_mat <- function(dat, var2, catx, cats, vars) {
  
  get_correlation <- function(var1, var2, df) {
    cor(select_(df, var1),
        select_(df, var2),
        method = "spearman",
        use = "pairwise.complete.obs"
    )
  }
  
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


# dataframe operations

filter_data_by_selections <- function(var2, catx, cats, vars) {
  panimmune_data$df %>%
    as_data_frame() %>%
    filter(UQ(as.name(catx)) %in% cats) %>%
    select_(.dots = c(catx, var2, vars))
}


