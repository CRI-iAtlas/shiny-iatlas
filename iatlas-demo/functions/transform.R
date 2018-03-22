
buildDataFrame_corr <- function(dat, var1, var2, catx) {
  
  # get the vectors associated with each term
  cats <- get_category_group(catx)
  vars <- get_variable_group(var1)
  
  # this would be the correlations
  cormat <- matrix(data = 0, ncol = length(cats), nrow = length(vars))
  rownames(cormat) <- vars
  colnames(cormat) <- cats
  # for each factor in catx
  for (ci in cats) {
    # subset dat
    subdat <- dat[dat[, catx] == ci, ]
    # compute correlation
    for (vi in vars) {
      cormat[vi, ci] <- cor(subdat[, vi], subdat[, var2],
                            method = "spearman",
                            use = "pairwise.complete.obs"
      )
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

# immunomodulator helpers -----------------------------------------------------

create_im_gene_histplot_df <- function(
  boxplot_df, boxplot_column, boxplot_selected_group) {
  filter(boxplot_df, UQ(as.name(boxplot_column)) == boxplot_selected_group)
}

create_im_gene_boxplot_df <- function(im_choice, ss_group) {
  panimmune_data$immunomodulator_df %>%
    filter(Symbol == im_choice) %>%
    left_join(panimmune_data$df) %>%
    mutate(log_count = log10(normalized_count + 1)) %>%
    select(ss_group, log_count) %>%
    .[complete.cases(.), ]
}

create_im_gene_histplot_df <- function(
  boxplot_df, boxplot_column, boxplot_selected_group) {
  filter(boxplot_df, UQ(as.name(boxplot_column)) == boxplot_selected_group)
}


# feature correlation helpers -------------------------------------------------

filter_data_by_selections <- function(var2, catx, cats, vars) {
  panimmune_data$df %>%
    as_data_frame() %>%
    filter(UQ(as.name(catx)) %in% cats) %>%
    select_(.dots = c(catx, var2, vars))
}

create_correlation_matrix <- function(dat, var2, catx, cats, vars) {
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

get_correlation <- function(var1, var2, df) {
  cor(select_(df, var1),
      select_(df, var2),
      method = "spearman",
      use = "pairwise.complete.obs"
  )
}