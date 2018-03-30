## Transform functions are used globally and within individual modules to
## subset, arrange, or otherwise modify data prior to visualization with tables
## or plots

# Global data transform ----

# ** PanImmune data subsetting ----

subset_panimmune_df <- function(
  df = panimmune_data$fmx_df, group_col, study_subtype
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
        
        df %>% 
          filter(COL %in% sample_groups) %>% 
          mutate(COL = fct_drop(COL))
      }
    )
  }
}

# Generic plot data transform ----

#' Format a dataframe for plotting summary values (count, sum, mean, etc.) for
#' different groups with bar plots; grouping is allowed at one to three levels:
#' group (required), subgroup, and facet
#'
#' @param df a tidy dataframe
#' @param group_column string name of column containing values to summarize; 
#'     will correspond to size of bars in the plot
#' @param group_column string name of top level column by which to group; will
#'     correspond to x- or y-axis ticks in plot
#' @param subgroup_column string name of second level column by which to group;
#'     will correspond to fill colors of individual bars/segments
#' @param facet_column string name of third level column by which to group;
#'     will correspond to subplots
#' @param operation string or vector of strings indicating which summary 
#'     statistic(s) to calculate
#'
#' @return
#' @export
#'
#' @examples
create_barplot_df <- function(
  df, value_column, group_column, subgroup_column = NULL, 
  facet_column = NULL, operations = c("sum", "mean", "sd", "se"), 
  add_label = FALSE
) {
  se <- function(x) { mean(x) / sqrt(length(x)) }
  
  df %>% 
    group_by(.dots = c(group_column, subgroup_column, facet_column)) %>% 
    summarise_at(value_column, .funs = operations) %>% 
    ungroup
}

#' Format a dataframe for plotting values of one column versus values of a
#' second column as points in a scatter plot
#'
#' @param df a tidy dataframe
#' @param filter_column string name of column on which to filter values to 
#'     subset rows
#' @param filter_value string representing value by which to filter rows 
#' @param x_column string name of column to use for x-axis
#' @param y_column string name of column to use for y-axis
#'
#' @return
#' @export
#'
#' @examples
create_scatterplot_df <- function(
  df, filter_column, filter_value, x_column, y_column
) {
  df %>%
    filter(UQ(as.name(filter_column)) == filter_value) %>%
    select_(.dots = x_column, y_column)
}

# Module specific data transform ----

# ** Sample groups overview module ----

build_sample_group_key_df <- function(df, sample_group_option) {
  decide_plot_colors(panimmune_data, sample_group_option) %>% 
    enframe() %>% 
    filter(name %in% df[[sample_group_option]]) %>% 
    left_join(
      panimmune_data$sample_group_df, 
      by = c("name" = "FeatureValue")
    ) %>% 
    distinct() %>% 
    mutate(
      `Group Size` = map_int(
        name, ~ sum(df[, sample_group_option] == ., na.rm = TRUE)
      )
    ) %>% 
    select(`Sample Group` = name, `Group Name` = FeatureName,
           `Group Size`, Characteristics, `Plot Color` = value)
}

build_mosaic_plot_df <- function(df, x_column, y_column, study_value) {
  let(
    alias = c(xvar = x_column,
              yvar = y_column),
    df %>%
      subset_panimmune_df(
        group_col = x_column, 
        study_subtype = study_value
      ) %>% 
      select(xvar, yvar) %>%
      .[complete.cases(.),] %>%
      mutate(xvar = as.factor(xvar)) %>%
      mutate(yvar = as.factor(yvar)))
}

# ** Immune feature trends module ----

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

# ** Tumor composition module ----

create_cell_fraction_df <- function(
  subset_df, group_column, cell_fraction_columns
) {
  let(
    alias = c(group_col = group_column),
    panimmune_data$fmx_df %>%
      select(group_col, cell_fraction_columns) %>%
      .[complete.cases(.), ] %>% 
      gather(fraction_name, fraction, -group_col)
  )
}

create_tumor_content_df <- function(subset_df, group_column) {
  let(
    alias = c(group_col = group_column),
    subset_df %>% 
      select(group_col, Stromal_Fraction, leukocyte_fraction) %>% 
      .[complete.cases(.),] %>% 
      mutate(Tumor_Fraction = 1 - Stromal_Fraction) %>% 
      gather(fraction_name, fraction, -group_col) %>% 
      mutate(fraction_name = str_replace(fraction_name, "Stromal_Fraction", "Stromal Fraction")) %>% 
      mutate(fraction_name = str_replace(fraction_name, "leukocyte_fraction", "Leukocyte Fraction")) %>% 
      mutate(fraction_name = str_replace(fraction_name, "Tumor_Fraction", "Tumor Fraction"))
  )
}

# ** Clinical outcomes module ----

build_survival_df <- function(df, group_column, time_column, k) {
  get_groups <- function(df, group_column, k) {
    if (group_column %in% c("Subtype_Immune_Model_Based")) {
      # then we don't need to produce catagories.
      as.character(df[[group_column]])
    }
    else {
      as.character(cut(df[[group_column]], k, ordered_result = T))
    }
  }
  
  # get the vectors associated with each term
  # if facet_column is already a catagory, just use that.
  # otherwise it needs to be divided into k catagories.
  groups <- get_groups(df, group_column, k)
  
  if (time_column == "OS_time") {
    status_column <- "OS"
  } else {
    status_column <- "PFI_1"
  }
  
  data.frame(
    status = pluck(df, status_column), 
    time = pluck(df, time_column),
    variable = groups, 
    measure = pluck(df, group_column)
  ) %>% 
    na.omit()
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

build_im_expr_plot_df <- function(df, im_option, sample_group_option) {
  panimmune_data$im_expr_df %>%
    filter(Symbol == im_option) %>%
    left_join(df) %>%
    mutate(log_count = log10(normalized_count + 1)) %>%
    select(sample_group_option, log_count) %>%
    .[complete.cases(.), ]
}

build_histogram_df <- function(df, filter_column, filter_value) {
  df %>%
    filter(UQ(as.name(filter_column)) == filter_value)
  
}






