## Transform functions are used globally and within individual modules to
## subset, arrange, or otherwise modify data prior to visualization with tables
## or plots

# Global data transform ----

# ** PanImmune data subsetting ----

subset_panimmune_df <- function(
    df = panimmune_data$fmx_df, 
    group_column, 
    study_option,
    user_group_df = NULL) {
    
    if (group_column %in% c("Subtype_Immune_Model_Based", "Study")) {
        return(df)
    } else if (group_column == "Subtype_Curated_Malta_Noushmehr_et_al") {
        return(subset_panimmune_df_by_TCGA_subtype(df, group_column, study_option))
    } else {
        return(subset_panimmune_df_by_user_groups(df, user_group_df, group_column)) 
    }
}

subset_panimmune_df_by_TCGA_subtype <- function(df, group_column, study_option){
    sample_groups <- panimmune_data$sample_group_df %>% 
        filter(sample_group == "tcga_subtype", 
               `TCGA Studies` %in% study_option) %>% 
        extract2("FeatureValue")
    
    wrapr::let(
        alias = c(COL = group_column), {
            
            df %>% 
                filter(COL %in% sample_groups) %>% 
                mutate(COL = fct_drop(COL))
        }
    )
}

subset_panimmune_df_by_user_groups <- function(df, user_group_df, group_column){
    wrapr::let(
        alias = c(
            COL1 = colnames(user_group_df[1]),
            COL2 = group_column), {
                user_group_df %>% 
                    select(COL1, COL2) %>% 
                    rename("ParticipantBarcode" = COL1) %>% 
                    inner_join(df) %>% 
                    filter(!is.na(COL2))
        }
    )
}


# Generic plot data transform ----

create_label <- function(
    df, title, name_column, group_column, value_columns
) {
    
    let(
        alias = c(namevar = name_column, groupvar = group_column),
        df %>%
            mutate(
                label = str_glue(
                    "<b>{title}:</b> {name} ({group})", 
                    title = title,
                    name = namevar, 
                    group = groupvar
                )) %>% 
            gather(value_name, value, one_of(value_columns)) %>% 
            mutate(
                value_label = str_glue(
                    "{name}: {value}", 
                    name = str_to_upper(value_name), 
                    value = sprintf("%0.3f", value)
                )
            ) %>% 
            group_by(label) %>% 
            mutate(value_label = str_c(value_label, collapse = "</br>")) %>% 
            ungroup() %>% 
            spread(value_name, value) %>% 
            unite(label, label, value_label, sep = "</br></br>")
    )
}

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
build_barplot_df <- function(
    df, x_column, y_column, color_column = NULL, facet_column = NULL, 
    operations = c("sum", "mean", "sd", "se"), add_label = FALSE
) {
    se <- function(x) { mean(x) / sqrt(length(x)) }
    
    df <- df %>% 
        group_by(.dots = c(x_column, color_column, facet_column)) %>% 
        summarise_at(y_column, .funs = operations) %>% 
        ungroup()
    if (add_label) {
        df %>%
            create_label(
                title = str_to_title(y_column),
                name_column = x_column,
                group_column = color_column,
                value_columns = operations
            )
    } else {
        df
    }
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
build_scatterplot_df <- function(
    df, filter_column, filter_value, x_column, y_column, 
    id_column = "ParticipantBarcode"
) {
    df %>%
        filter(UQ(as.name(filter_column)) == filter_value) %>%
        create_label(
            title = id_column, 
            name_column = id_column, 
            group_column = filter_column, 
            value_columns = c(x_column, y_column)
        ) %>% 
        select_(.dots = id_column, x_column, y_column, "label")
}

# Module specific data transform ----

# ** Sample groups overview module ----

build_sample_group_key_df <- function(
    subset_df, group_option, plot_colors, 
    feature_df = panimmune_data$sample_group_df) {

    color_df <- build_plot_color_df(plot_colors, subset_df[[group_option]]) 
    if(nrow(color_df) == 0){
        stop("No matching members in groups between color list, and group df")
    }
    
    group_size_df <- build_group_size_df(subset_df, group_option) 
    if(nrow(group_size_df) == 0){
        stop("Group column has no members")
    }
    
    feature_df <- dplyr::select(
            feature_df,
            Group = FeatureValue,
            FeatureName,
            Characteristics)
    
    key_df <- 
        list(feature_df, color_df, group_size_df) %>% 
        dplyr::reduce(dplyr::inner_join, by = "Group") %>% 
        dplyr::distinct() %>% 
        dplyr::select(
            `Sample Group` = Group, 
            `Group Name` = FeatureName,
            `Group Size` = Group_Size, 
            Characteristics, 
            `Plot Color` = Plot_Color)
    
    if(nrow(key_df) == 0){
        stop("Result group key df is empty")
    }
}

build_plot_color_df <- function(color_vector, groups){
    color_vector %>% 
        tibble::enframe() %>% 
        magrittr::set_names(c("Group", "Plot_Color")) %>% 
        dplyr::filter(Group %in% groups)
}

build_group_size_df <- function(df, group_col){
    df %>% 
        dplyr::select(Group = group_col) %>% 
        tidyr::drop_na() %>% 
        dplyr::group_by(Group) %>% 
        dplyr::summarise(Group_Size = n()) 
}


build_mosaic_plot_df <- function(
    df, x_column, y_column, study_option, user_group_df = NULL) {

    df %>%
        subset_panimmune_df(
            group_col = x_column, 
            study_option = study_option,
            user_group_df) %>% 
        dplyr::select(
            x = x_column,
            y = y_column) %>%
        tidyr::drop_na() %>% 
        dplyr::mutate(x = as.factor(x)) %>%
        dplyr::mutate(y = forcats::fct_rev(as.factor(y))) %>% 
        magrittr::set_colnames(c(x_column, y_column))
}

# ** Immune feature trends module ----

build_intermediate_corr_df <- function(
    df, value1_column, group_column, group_options, value2_columns,
    id_column = "ParticipantBarcode" ) {
    
    wrapr::let(
        c(GROUP = group_column),
        result_df <- df %>% 
            dplyr::select(id_column, GROUP, value1_column, value2_columns) %>% 
            dplyr::filter(GROUP %in% group_options))
}


build_heatmap_corr_mat <- function(
    df, group_column, value1_column, value2_columns) {

    df %>% 
        dplyr::select(
            group = group_column,
            value1 = value1_column,
            value2_columns) %>% 
        tidyr::gather(key = "variable", value = "value2", -c(group, value1)) %>% 
        dplyr::group_by(group, variable) %>% 
        dplyr::summarise(cor = cor(
            value1, 
            value2,
            method = "spearman", 
            use = "pairwise.complete.obs")) %>% 
        tidyr::spread(key = "group", value = "cor", fill = 0) %>% 
        dplyr::mutate(variable = map(variable, get_variable_display_name)) %>% 
        as.data.frame %>% 
        tibble::column_to_rownames("variable") %>% 
        as.matrix()
}


# ** Tumor composition module ----

build_cell_fraction_df <- function(df, group_column, value_columns){
    df %>% 
        dplyr::select(GROUP = group_column, value_columns) %>% 
        .[complete.cases(.),] %>% 
        tidyr::gather(fraction_type, fraction, -GROUP)
}

build_tumor_content_df <- function(df, group_column){
    df %>% 
        dplyr::select(
            GROUP = group_column,
            "Stromal_Fraction", 
            "leukocyte_fraction") %>% 
        .[complete.cases(.),] %>% 
        dplyr::mutate(Tumor_Fraction = 1 - Stromal_Fraction) %>% 
        magrittr::set_colnames(c(
            "GROUP", 
            "Stromal Fraction", 
            "Leukocyte Fraction", 
            "Tumor Fraction")) %>% 
        tidyr::gather(fraction_type, fraction, -GROUP)
}

# ** Clinical outcomes module ----

build_survival_df <- function(df, group_column, group_options, time_column, k) {
    get_groups <- function(df, group_column, k) {
        if (group_column %in% group_options) {
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



get_concordance <- function(
    df, value_column, time_column, status_column
) {
    wrapr::let(
        alias = c(valuevar = value_column,
                  timevar = time_column,
                  statusvar = status_column),
        mat <- df %>% 
            select(valuevar, timevar, statusvar) %>% 
            .[complete.cases(.),] %>% 
            as.data.frame() %>% 
            as.matrix()
    )
    
    concordanceIndex::concordanceIndex(mat[,1], mat[,-1])
}

get_concordance_by_group <- function(
    df, value_columns, time_column, status_column
) {
    value_columns %>% 
        map(function(f) get_concordance(df, f, time_column, status_column)) %>% 
        set_names(value_columns)
}

build_ci_mat <- function(
    df, group_column, value_columns, time_column, status_column
) {
    value_names <- map(value_columns, get_variable_display_name)
    group_v <- extract2(df, group_column) 
    groups <- group_v %>% 
        unique() %>% 
        discard(is.na(.)) %>% 
        sort()
    
    df %>% 
        split(group_v) %>% 
        map(get_concordance_by_group, value_columns, time_column, status_column) %>% 
        unlist() %>% 
        unname() %>% 
        matrix(ncol = length(groups)) %>% 
        set_rownames(value_names) %>% 
        set_colnames(groups)
}

# ** Immune interface module ----

build_immuneinterface_df <- function(
    build_df, sample_group, diversity_vars
) {
    df %>%
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

build_im_expr_plot_df <- function(df, filter_value, group_option) {
    panimmune_data$im_expr_df %>%
        filter(Symbol == filter_value) %>%
        left_join(df) %>%
        mutate(log_count = log10(normalized_count + 1)) %>%
        select(group_option, log_count) %>%
        .[complete.cases(.), ]
}

