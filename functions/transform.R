## Transform functions are used globally and within individual modules to
## subset, arrange, or otherwise modify data prior to visualization with tables
## or plots

###############################################################################
# These functions have been refactored and have unit tests.
# Do not make any modifications to these!
# If you want to make a modification, please copy and paste the function the
# lower section and call it <function_name>2.
# Make any needed modifcations to the coipied function.
# The new functionality will get unut tests and be folded back into the 
# original function.
###############################################################################

# immunomodulator functions ---------------------------------------------------

build_immunomodulator_expression_df <- function(
    group_df, filter_value, group_col,
    expression_df = panimmune_data$im_expr_df,
    expression_filter_col = "Symbol",
    expression_col = "normalized_count",
    id_col = "ParticipantBarcode"){
    
    expression_df <- filter_immunomodulator_expression_df(
        expression_df, 
        id_col, 
        expression_filter_col, 
        expression_col, 
        filter_value)
    
    group_df <- group_df %>% 
        get_complete_df_by_columns(c(group_col, id_col)) %>% 
        select(GROUP = group_col, ID = id_col)
    
    result_df <- 
        dplyr::inner_join(group_df, expression_df, by = "ID") %>%
        dplyr::select(GROUP, LOG_COUNT)
}

filter_immunomodulator_expression_df <- function(
    df, id_col, filter_col, expression_col, filter_value){
    
    df %>% 
        get_complete_df_by_columns(c(
            id_col, 
            filter_col, 
            expression_col)) %>% 
        dplyr::select(
            FILTER = filter_col, 
            COUNT = expression_col,
            ID = id_col) %>% 
        dplyr::filter(FILTER == filter_value) %>% 
        dplyr::mutate(LOG_COUNT = log10(COUNT + 1)) %>% 
        dplyr::select(LOG_COUNT, ID)
}


build_immunomodulator_violin_plot_df <- function(df){
    df %>%
        dplyr::select(x = GROUP, y = LOG_COUNT) %>% 
        get_complete_df_by_columns(c("x", "y"))
}

build_immunomodulator_histogram_df <- function(df, selected_group){
    df %>%
        filter(GROUP == selected_group) %>% 
        select(x = LOG_COUNT) %>% 
        get_complete_df_by_columns("x")
}

# immunefeatures functions ----------------------------------------------------

build_immunefeatures_df <- function(
    df, 
    group_column, 
    value1_column, 
    value2_columns,
    group_options, 
    id_column = "ParticipantBarcode"){
    
    assert_df_has_columns(
        df, c(group_column, value1_column, value2_columns, id_column))
    
    result_df <- df %>% 
        dplyr::select(
            ID = id_column, 
            GROUP = group_column, 
            VALUE1 = value1_column, 
            value2_columns) %>% 
        dplyr::filter(GROUP %in% group_options)
    assert_df_has_rows(result_df)
    return(result_df)
}

build_immunefeatures_correlation_matrix <- function(df, method = "spearman") {
    df  %>% 
        dplyr::select(-ID) %>% 
        tidyr::gather(
            key = "VARIABLE", 
            value = "VALUE2", 
            -c(GROUP, VALUE1)) %>% 
        dplyr::group_by(GROUP, VARIABLE) %>% 
        dplyr::summarise(COR = cor(
            VALUE1, 
            VALUE2,
            method = method, 
            use = "pairwise.complete.obs")) %>% 
        tidyr::spread(key = "GROUP", value = "COR", fill = 0) %>% 
        dplyr::mutate(VARIABLE = map(VARIABLE, get_variable_display_name)) %>% 
        as.data.frame() %>% 
        tibble::column_to_rownames("VARIABLE") %>% 
        as.matrix()
}


build_immunefeatures_violin_plot_df <- function(df, x_col, y_col){
    df %>%
        dplyr::select(x = x_col, y = y_col) %>% 
        get_complete_df_by_columns(c("x", "y"))
}

build_immunefeatures_scatter_plot_df <- function(df, x_col, group_filter_value){
    assert_df_has_columns(df, c(x_col, "VALUE1", "ID", "GROUP"))
    df %>%
        select(ID, GROUP, y = "VALUE1", x = x_col) %>% 
        filter(GROUP == group_filter_value) %>% 
        create_label(
            name_column = "ID",
            group_column = "GROUP",
            value_columns = c("x", "y")) %>%
        select("x", "y", "label") %>% 
        get_complete_df_by_columns(c("x", "y", "label"))
}


# cellcontent functions -------------------------------------------------------

build_cellcontent_df <- function(df, group_column){
    assert_df_has_columns(df, c(group_column, "Stromal_Fraction", "leukocyte_fraction"))
    result_df <- df %>% 
        dplyr::select(
            GROUP = group_column,
            "Stromal_Fraction", 
            "leukocyte_fraction") %>% 
        get_complete_df_by_columns(c("GROUP", "Stromal_Fraction", "leukocyte_fraction")) %>% 
        dplyr::mutate(Tumor_Fraction = 1 - Stromal_Fraction) %>% 
        magrittr::set_colnames(c(
            "GROUP", 
            "Stromal Fraction", 
            "Leukocyte Fraction", 
            "Tumor Fraction")) %>% 
        tidyr::gather(fraction_type, fraction, -GROUP)
    assert_df_has_columns(result_df, c("GROUP", "fraction_type", "fraction"))
    assert_df_has_rows(result_df)
    return(result_df)
}

build_cell_fraction_df <- function(df, group_column, value_columns){
    assert_df_has_columns(df, c(group_column, value_columns))
    result_df <- df %>% 
        dplyr::select(GROUP = group_column, value_columns) %>% 
        get_complete_df_by_columns(c("GROUP", value_columns)) %>% 
        tidyr::gather(fraction_type, fraction, -GROUP)
    assert_df_has_columns(result_df, c("GROUP", "fraction_type", "fraction"))
    assert_df_has_rows(result_df)
    return(result_df)
}


build_cellcontent_barplot_df <- function(
    df, x_column, y_column, 
    operations = c("sum", "mean", "sd", "se")) {
    
    
    assert_df_has_columns(df, c("GROUP", "fraction_type", "fraction"))
    result_df <- df %>%
        dplyr::group_by(GROUP, fraction_type) %>%
        dplyr::summarise_at("fraction", .funs = operations) %>%
        dplyr::ungroup() %>%
        create_label(
            title = stringr::str_to_title(y_column),
            name_column = x_column,
            group_column = "GROUP",
            value_columns = operations) %>% 
        dplyr::select(
            x = "GROUP",
            y = "mean",
            color = "fraction_type",
            error = "se",
            label)
    assert_df_has_columns(result_df, c("x", "y", "label", "color", "error"))
    assert_df_has_rows(result_df)
    return(result_df)
}

build_cellcontent_scatterplot_df <- function(
    df, group_column, group_filter_value, 
    id_column = "ParticipantBarcode",
    x_column = "Stromal_Fraction",
    y_column = "leukocyte_fraction") {
    
    result_df  <- df %>%
        select(
            GROUP = group_column, 
            ID = id_column, 
            x = x_column, 
            y = y_column) %>%
        filter(GROUP == group_filter_value) %>%
        create_label(
            name_column = "ID",
            group_column = "GROUP",
            value_columns = c("x", "y")) %>% 
        select(x, y, "label")
    assert_df_has_columns(result_df, c("x", "y", "label"))
    assert_df_has_rows(result_df)
    return(result_df)
}

# samplegroup functions -------------------------------------------------------

build_sample_group_key_df <- function(
    group_df, group_column, color_vector, 
    feature_df = panimmune_data$sample_group_df) {
    
    assert_df_has_columns(group_df, group_column)
    
    color_df <- build_plot_color_df(color_vector, group_df, group_column) 
    assert_df_has_rows(color_df)
    
    group_size_df <- build_group_size_df(group_df, group_column) 
    key_df <- build_key_df(feature_df, color_df, group_size_df)
    
    return(key_df)
}

build_plot_color_df <- function(color_vector, subset_df, group_column){
    result_df <- color_vector %>% 
        tibble::enframe() %>% 
        magrittr::set_names(c("Group", "Plot_Color")) %>% 
        dplyr::filter(Group %in% subset_df[[group_column]])
    assert_df_has_columns(result_df, c("Group", "Plot_Color"))
    assert_df_has_rows(result_df)
    return(result_df)
}

build_group_size_df <- function(df, group_col){
    assert_df_has_columns(df, group_col)
    result_df <- df %>% 
        dplyr::select(Group = group_col) %>% 
        get_complete_df_by_columns("Group") %>%  
        dplyr::group_by(Group) %>% 
        dplyr::summarise(Group_Size = n()) 
    assert_df_has_columns(result_df, c("Group", "Group_Size"))
    assert_df_has_rows(result_df)
    return(result_df)
}


build_key_df <- function(feature_df, color_df, group_size_df){
    
    assert_df_has_columns(
        feature_df, 
        c("FeatureValue", "FeatureName", "Characteristics"))
    
    
    feature_df <- dplyr::select(
        feature_df,
        Group = FeatureValue,
        FeatureName,
        Characteristics)
    
    key_df <- 
        dplyr::inner_join(color_df, group_size_df, by = "Group") %>% 
        dplyr::left_join(feature_df, by = "Group") %>% 
        dplyr::distinct() %>% 
        dplyr::select(
            `Sample Group` = Group, 
            `Group Name` = FeatureName,
            `Group Size` = Group_Size, 
            Characteristics, 
            `Plot Color` = Plot_Color)
    
    assert_df_has_columns(
        key_df, 
        c("Sample Group", "Group Name", "Group Size", "Characteristics", "Plot Color"))
    assert_df_has_rows(key_df)
    return(key_df)
}


# functions for making plot df label column -----------------------------------

create_label <- function(
    df,
    value_columns,
    title = "ParticipantBarcode",
    name_column = "name",
    group_column = "group") {
    
    result_df <- let(
        alias = c(
            namevar = name_column,
            groupvar = group_column),
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
    assert_df_has_columns(result_df, c("label", name_column, group_column, value_columns))
    assert_df_has_rows(result_df)
    return(result_df)
    
}


# other functions -------------------------------------------------------------

build_mosaicplot_df <- function(df, x_column, y_column){
    
    assert_df_has_columns(df, c(x_column, y_column))
    result_df <- df %>% 
        dplyr::select(x = x_column, y = y_column) %>% 
        get_complete_df_by_columns(c("x", "y")) %>% 
        dplyr::mutate(x = as.factor(x)) %>%
        dplyr::mutate(y = forcats::fct_rev(as.factor(y)))
    assert_df_has_columns(result_df, c("x", "y"))
    assert_df_has_rows(result_df)
    return(result_df)
}
###############################################################################
# Tests below this line do not have tests yet, newly writen functions 
###############################################################################
# Global data transform ----
# ** PanImmune data subsetting ----
# subset by group choices -----
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

# Module specific data transform ----

# ** Sample groups overview module ----

build_group_group_mosaic_plot_df <- function(
    df, x_column, y_column, study_option, user_group_df = NULL) {
    
    df %>%
        subset_panimmune_df(
            group_col = x_column, 
            study_option = study_option,
            user_group_df) %>% 
        build_mosaicplot_df(x_column, y_column) 
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

# build_immuneinterface_df <- function(
#     build_df, sample_group, diversity_vars
# ) {
#     df %>%
#         select(sample_group, diversity_vars) %>%
#         .[complete.cases(.), ] %>%
#         gather(metric, diversity, -1) %>%
#         separate(metric, into = c("receptor", "metric"), sep = "_")
# }
# 
# ztransform_df <- function(df) {
#     df %>%
#         group_by(receptor, metric) %>%
#         mutate(
#             div_mean = mean(diversity),
#             div_sd = sd(diversity)
#         ) %>%
#         ungroup() %>%
#         mutate(diversity = (diversity - div_mean) / div_sd)
# }

# ** Immunomodulators module ----



# ** Driver correlation module ----

##
##  Builds data frame used for the regression 
##

build_mutation_df <- function(df, response_var, group_column, group_options){
    fmx_df.intermediate <- build_intermediate_fmx_df_for_groups(
        df,
        response_var,
        group_column,
        group_options)
    driver_mutation_df.long <-
        panimmune_data$driver_mutation_df %>%
        tidyr::gather(key = "mutation", value = "value", -c("ParticipantBarcode")) %>%
        dplyr::mutate(value = fct_relevel(value, "Wt", "Mut"))
    mutation_df <- build_driver_mutation_df(driver_mutation_df.long, fmx_df.intermediate)
    if(nrow(mutation_df) == 0){
        mutation_df <- NULL
    } else {
        mutation_df <- label_driver_mutation_df(mutation_df, group_column)
    }
    return(mutation_df)
}


#
# join driver data frame and fmx (feature matrix) data frame 
#
build_driver_mutation_df <- function(driver_df, fmx_df) {
    driver_df %>%
        dplyr::left_join(fmx_df, by="ParticipantBarcode") %>% 
        tidyr::drop_na()
}

label_driver_mutation_df <- function(df, group_column){
    df_labeled <- wrapr::let(
        c(gc = group_column),
        dplyr::mutate(df, mutation_group = stringr::str_c(mutation, gc, sep = ".")))
}

## filter mutation data frame to mutations meeting a minimum overall count_threshold within a group
## count_threshold is the minimum mutation count required
## In rare cases, combinations where all samples, or all but one samples is mutations occur
## These are removed as well as significance testing cannot be performed

#
# For each combination of mutation and group, compile total count and count mutated
#
build_mutation_group_summary_df <- function(df){
    df %>%
        dplyr::mutate(value = ifelse(value == "Wt", 0, 1)) %>% 
        dplyr::select(mutation_group, value) %>%
        dplyr::group_by(mutation_group) %>%
        dplyr::summarise(
            mutation_count = sum(value),
            cat_count = dplyr::n()) %>%
        dplyr::ungroup()
}

#
# identify which mutation groups combination have sufficient data for test
#
# uses a universal minimum count. Better might be to use percent of group size as a minimum.
get_testable_mutation_groups <- function(df, count_threshold = 4){
    df %>%
        dplyr::filter(mutation_count >= count_threshold) %>% # requirement for a minimal mutation count
        dplyr::filter(mutation_count < cat_count - 1) %>% # cannot test if all mutated or all but one mutated
        magrittr::use_series(mutation_group)
}

get_untestable_mutation_groups <- function(df, testable_mutation_groups){
    df %>% 
        dplyr::filter(!mutation_group %in% testable_mutation_groups) %>% 
        magrittr::use_series(mutation_group)
}

##
## restrict fmx_df to rows with available group values and a single selected value column
##
build_intermediate_fmx_df_for_groups <- function(
    df, value_column, group_column, group_options, id_column = "ParticipantBarcode" ) {
    wrapr::let(
        c(GROUP = group_column),
        result_df <- df %>% 
            dplyr::select(id_column, GROUP, value_column) %>% 
            dplyr::filter(GROUP %in% group_options)) %>% .[complete.cases(.),]    
}

##
## filter mutation df to mutations meeting a minimum overall count_threshold
## currently not used, will need for "full pancan" mode (test before using)
##
build_filtered_mutation_df_pancan <- function(df,count_threshold=80){   # select greater than 1% mutation for now
  binvec <- c(0,1) ; names(binvec) <- c("Wt","Mut")
  df.boole <- df  %>% 
    mutate(boole=as.vector(binvec[.$value])) %>% select(-value) %>% rename(value=boole)
  driver_mutation.mutcount <-  df.boole %>% select(-ParticipantBarcode) %>% 
    dplyr::group_by(mutation) %>%  dplyr::summarise(mutation_count = sum(value)) %>% ungroup()
  drivers.keep <- driver_mutation.mutcount %>% filter(mutation_count > count_threshold)  %>% .$mutation
  df %>% filter(mutation %in% drivers.keep)
}






##
## Compute p values for each 'combo' of driver mutation and cohort
##
compute_pvals_per_combo <- function(df, value_column, group_column){
    result_vec <- wrapr::let(
        c(response_var=value_column,
          gc=group_column),
        df %>% 
            split(.$mutation_group) %>% 
            map( ~ lm(response_var ~ value, data=.)) %>%
            map(summary) %>%
            map("coefficients") %>% 
            map(~. ["valueMut","Pr(>|t|)"]) %>%
            unlist()
    )
    
    data.frame(
        mutation_group=as.vector(names(result_vec)),
        neglog_pval=as.vector(-log10(result_vec)),
        stringsAsFactors = FALSE)
}

##
## Compute effect size  for each 'combo' of driver mutation and cohort
##

compute_effect_size_per_combo <- function(df, value_column, group_column){
    wrapr::let(
        c(response_var=value_column,gc=group_column),
        df_means <- df %>% 
            group_by(mutation_group,value) %>%
            summarize(mean_response=mean(response_var)) %>%
            spread(value,mean_response) %>%
            mutate(effect_size=-log10(Wt/Mut)) %>%
            select(-c(Wt,Mut)) %>% as.data.frame
    )
}




##
## Compute p-value and effect size for each combo and combine to single data frame
##
compute_driver_associations <- function(df_for_regression,response_var,group_column,group_options){
    res1 <- compute_pvals_per_combo(df_for_regression,response_var, group_column)
    res2 <- compute_effect_size_per_combo(df_for_regression,response_var, group_column)
    inner_join(res1,res2,by="mutation_group") ## returns df with combo,neglog_pval,effect_size
}
