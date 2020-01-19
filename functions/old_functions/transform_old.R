## Transform functions are used globally and within individual modules to
## subset, arrange, or otherwise modify data prior to visualization with tables
## or plots


# Driver mutation functions ---------------------------------------------------

build_mv_driver_mutation_tbl <- function(
    subset_metric_tbl,
    response_column,
    covariate_columns,
    group_mode,
    group_column,
    n_wt_min, 
    n_mut_min,
    scale_response,
    driver_tbl = panimmune_data$driver_mutations_df,
    metric_tbl = panimmune_data$fmx_df
){
    select_columns <- c("ParticipantBarcode", response_column, covariate_columns)
    label_columns <-  c("GENE")
    if(group_mode == "By group"){
        select_columns <- c(select_columns, group_column)
        label_columns  <- c(label_columns, group_column)
        metric_tbl <- subset_metric_tbl
    }
    if(scale_response){
        print(metric_tbl)
        metric_tbl[response_column] <- scale(metric_tbl[response_column])
        print(metric_tbl)
    }
    assert_df_has_columns(metric_tbl, select_columns)
    driver_tbl <- tidyr::gather(driver_tbl, GENE, STATUS, -ParticipantBarcode)
    result_tbl <-
        combine_metric_and_driver_tbl(
            metric_tbl, 
            driver_tbl,
            response_column,
            select_columns,
            label_columns,
            covariate_columns
        ) %>% 
        filter_driver_tbl_by_group_size(n_wt_min, n_mut_min) 
}

combine_metric_and_driver_tbl <- function(
    metric_tbl, 
    driver_tbl,
    response_column,
    select_columns,
    label_columns,
    covariate_columns,
    join_columns = c("ParticipantBarcode")
){
    metric_tbl %>%
        dplyr::select(select_columns) %>% 
        dplyr::rename(RESPONSE = response_column) %>% 
        tidyr::drop_na() %>% 
        dplyr::inner_join(driver_tbl, by = join_columns) %>% 
        tidyr::drop_na() %>%
        tidyr::unite(LABEL, label_columns, sep = ":")  %>%
        dplyr::select(LABEL, RESPONSE, STATUS, covariate_columns)
    
}

filter_driver_tbl_by_group_size <- function(driver_tbl, n_wt_min, n_mut_min){
    group_size_tbl <- driver_tbl %>%
        dplyr::group_by(LABEL) %>% 
        dplyr::summarise(
            n_total = dplyr::n(),
            n_wt = sum(STATUS == "Wt"),
            n_mut = n_total - n_wt
        ) %>% 
        dplyr::filter(n_mut >= n_mut_min & n_wt >= n_wt_min) %>% 
        dplyr::ungroup() %>% 
        dplyr::select(-c(n_total, n_mut, n_wt))
    dplyr::inner_join(driver_tbl, group_size_tbl)
 
}


build_mv_driver_mutation_scatterplot_df <- function(
    driver_tbl, 
    covariates,
    model_formula
){
    driver_tbl %>% 
        add_effect_size(
            "Wt", 
            "Mut", 
            value_column = "RESPONSE",
            group_column = "STATUS") %>% 
        add_driver_pvalues(covariates, model_formula) %>% 
        dplyr::select(y = PVALUE, x = EFFECT_SIZE, label = LABEL) %>% 
        dplyr::mutate(color = dplyr::if_else(
            y < 1.3 | abs(x) < 0.1,
            "blue",
            "red"
        ))
}

add_effect_size <- function(
    tbl, 
    group1, 
    group2, 
    value_column = "VALUE",
    label_column = "LABEL",
    group_column = "GROUP",
    method = log_ratio_effect_size
){
    tbl %>% 
        dplyr::select(
            GROUP = group_column, 
            LABEL = label_column,
            VALUE = value_column
        ) %>% 
        dplyr::group_by(LABEL, GROUP) %>%
        dplyr::summarise(VALUES = list(VALUE)) %>% 
        tidyr::spread(GROUP, VALUES) %>%
        dplyr::rename(GROUP1 = group1, GROUP2 = group2) %>% 
        tidyr::nest(GROUP1, GROUP2, .key = DATA) %>% 
        dplyr::mutate(EFFECT_SIZE = as.double(parallel::mclapply(
            DATA, 
            get_effect_size_from_df,
            method
        ))) %>% 
        dplyr::select(-DATA) %>% 
        tidyr::drop_na() %>% 
        dplyr::inner_join(tbl, by = label_column)
}



add_driver_pvalues <- function(driver_tbl, covariates, model_formula){
    # this forces the formula variable to evaluate before calling mclapply
    force(model_formula)
    
    driver_tbl %>% 
        tidyr::nest(RESPONSE, STATUS, covariates, .key = DATA) %>%
        dplyr::mutate(PVALUE = as.double(parallel::mclapply(
            DATA,
            calculate_lm_pvalue,
            model_formula,
            "STATUSWt"
        ))) %>%
        dplyr::select(-DATA) %>% 
        dplyr::mutate(PVALUE = -log10(PVALUE))
}



# samplegroup functions -------------------------------------------------------
# 
# build_sample_group_key_df <- function(group_df, group_column, feature_df) {
#     assert_df_has_columns(group_df, group_column)
#     
#     group_size_df <- build_group_size_df(group_df, group_column) 
#     
#     result_df <- feature_df %>% 
#         dplyr::select(
#             Group = FeatureValue,
#             FeatureName,
#             Characteristics,
#             FeatureHex) %>% 
#         dplyr::inner_join(group_size_df, by = "Group") %>% 
#         dplyr::distinct() %>% 
#         dplyr::select(
#             `Sample Group` = Group, 
#             `Group Name` = FeatureName,
#             `Group Size` = Group_Size, 
#             Characteristics, 
#             `Plot Color` = FeatureHex)
#     assert_df_has_columns(
#         result_df, 
#         c("Sample Group", "Group Name", "Group Size", "Characteristics", "Plot Color"))
#     assert_df_has_rows(result_df)
#     return(result_df)
# }
# 
# build_group_size_df <- function(df, group_col){
#     assert_df_has_columns(df, group_col)
#     result_df <- df %>% 
#         dplyr::select(Group = group_col) %>% 
#         get_complete_df_by_columns("Group") %>%  
#         dplyr::group_by(Group) %>% 
#         dplyr::summarise(Group_Size = n()) 
#     assert_df_has_columns(result_df, c("Group", "Group_Size"))
#     assert_df_has_rows(result_df)
#     return(result_df)
# }
# 
# # functions for making plot df label column -----------------------------------
# 
# create_label <- function(
#     df,
#     value_columns,
#     title = "ParticipantBarcode",
#     name_column = "name",
#     group_column = "group") {
#     
#     result_df <- wrapr::let(
#         alias = c(
#             namevar = name_column,
#             groupvar = group_column),
#         df %>%
#             dplyr::mutate(
#                 label = stringr::str_glue(
#                     "<b>{title}:</b> {name} ({group})",
#                     title = title,
#                     name = namevar,
#                     group = groupvar
#                 )) %>% 
#             tidyr::gather(value_name, value, dplyr::one_of(value_columns)) %>%
#             dplyr::mutate(
#                 value_label = stringr::str_glue(
#                     "{name}: {value}",
#                     name = stringr::str_to_upper(value_name),
#                     value = sprintf("%0.3f", value)
#                 )
#             ) %>%
#             dplyr::group_by(label) %>%
#             dplyr::mutate(value_label = stringr::str_c(value_label, collapse = "</br>")) %>%
#             ungroup() %>%
#             tidyr::spread(value_name, value) %>%
#             tidyr::unite(label, label, value_label, sep = "</br></br>")
#     )
#     assert_df_has_columns(result_df, c("label", name_column, group_column, value_columns))
#     assert_df_has_rows(result_df)
#     return(result_df)
#     
# }
# 
# 
# # other functions -------------------------------------------------------------
# 
# subset_sample_group_df <- function(
#     group_column, study_option, user_group_df,
#     sample_group_df = panimmune_data$sample_group_df
# ){
#     if (group_column %in% c("Subtype_Immune_Model_Based", "Study")) {
#         sample_group_df <- dplyr::filter(sample_group_df, sample_group == group_column)
#         
#     } else if (group_column == "Subtype_Curated_Malta_Noushmehr_et_al") {
#         if(is.null(study_option)) return(NULL)
#         sample_group_df <- sample_group_df %>%
#             dplyr::filter(sample_group == group_column) %>%
#             dplyr::filter(`TCGA Studies` == study_option)
#     } else {
#         sample_group_df <- user_group_df %>% 
#             user_group_df_to_sample_group_df() %>% 
#             dplyr::filter(sample_group == group_column) %>%
#             add_missing_plot_colors()
#     }
#     return(sample_group_df)
# 
# }
# 
# user_group_df_to_sample_group_df <- function(df){
#     df %>% 
#         tidyr::gather(key = 'sample_group' , value = "FeatureValue", -1) %>% 
#         dplyr::select(-(1)) %>% 
#         dplyr::distinct() %>% 
#         dplyr::mutate(FeatureName = FeatureValue) %>% 
#         dplyr::mutate(Characteristics = "") %>% 
#         dplyr::mutate(FeatureHex = NA) %>% 
#         dplyr::mutate(`TCGA Studies` = NA)
# }
# 
# add_missing_plot_colors <- function(df){
#     result_df <- df %>% 
#         dplyr::group_by(`TCGA Studies`) %>% 
#         dplyr::mutate(
#             FeatureHex = suppressWarnings(
#                 ifelse(
#                     is.na(FeatureHex),
#                     RColorBrewer::brewer.pal(
#                         length(FeatureValue), "Set1") %>%
#                         .[1:length(FeatureValue)],
#                     FeatureHex)
#             )
#         ) %>% 
#         dplyr::ungroup()
#     return(result_df)
# }
# 
# translate_to_correct_group_name <- function(df){
#     df %>% 
#         dplyr::mutate(
#             sample_group = dplyr::if_else(
#                 sample_group == "tcga_study", 
#                 "Study",
#                 dplyr::if_else(
#                     sample_group == "immune_subtype", 
#                     "Subtype_Immune_Model_Based",
#                     "Subtype_Curated_Malta_Noushmehr_et_al")))
# }
# 
# summarise_df_at_column <- function(df, column, grouping_columns, function_names){
#     assert_df_has_columns(df, c(column, grouping_columns))
#     result_df <- df %>% 
#         dplyr::group_by_at(vars(dplyr::one_of(grouping_columns))) %>%
#         dplyr::summarise_at(column, .funs = function_names) %>%
#         dplyr::ungroup() 
#     if(length(function_names) == 1){
#         result_df <- dplyr::rename(result_df, !!function_names := column)
#     }
#     assert_df_has_columns(result_df, c(grouping_columns, function_names))
#     assert_df_has_rows(result_df)
#     return(result_df)
# }
# 
# 
# 
# build_group_group_mosaic_plot_df <- function(
#     df, x_column, y_column, user_group_df, sample_group_df, study_subset_selection) {
# 
#     sample_group_df <- subset_sample_group_df(
#         x_column, 
#         study_subset_selection, 
#         user_group_df)
#     
#     subset_df <- subset_panimmune_df(
#         group_column = x_column, 
#         user_group_df = user_group_df,
#         sample_group_df = sample_group_df,
#         df = df
#     )
#     
#     result_df <- build_mosaicplot_df(subset_df, x_column, y_column) 
# }
# 
# 
# subset_panimmune_df <- function(
#     group_column, user_group_df, sample_group_df,
#     df = panimmune_data$fmx_df
#     ) {
#     
#     if (!group_column %in% unlist(config_yaml$immune_groups)) {
#         df <- subset_panimmune_df_by_user_groups(df, user_group_df, group_column)
#     }
#     
#     result_df <- wrapr::let(
#         alias = c(COL = group_column),
#         dplyr::filter(df, COL %in% sample_group_df$FeatureValue)
#     )
#     return(result_df)
# }
# 
# subset_panimmune_df_by_user_groups <- function(df, user_group_df, group_column){
#     wrapr::let(
#         alias = c(
#             COL1 = colnames(user_group_df[1]),
#             COL2 = group_column), {
#                 user_group_df %>% 
#                     dplyr::select(COL1, COL2) %>% 
#                     dplyr::rename("ParticipantBarcode" = COL1) %>% 
#                     dplyr::inner_join(df) 
#             }
#     )
# }
# 
# build_mosaicplot_df <- function(df, x_column, y_column){
#     
#     assert_df_has_columns(df, c(x_column, y_column))
#     result_df <- df %>% 
#         dplyr::select(x = x_column, y = y_column) %>% 
#         tidyr::drop_na() %>% 
#         dplyr::mutate(x = as.factor(x)) %>%
#         dplyr::mutate(y = forcats::fct_rev(as.factor(y)))
#     assert_df_has_columns(result_df, c("x", "y"))
#     return(result_df)
# }
# 
# 

# ** Clinical outcomes module ----

# build_survival_df <- function(df, group_column, group_options, time_column, k) {
#     get_groups <- function(df, group_column, k) {
#         if (group_column %in% group_options) {
#             # then we don't need to produce catagories.
#             as.character(df[[group_column]])
#         }
#         else {
#             as.character(cut(df[[group_column]], k, ordered_result = T))
#         }
#     }
#     
#     # get the vectors associated with each term
#     # if facet_column is already a catagory, just use that.
#     # otherwise it needs to be divided into k catagories.
#     groups <- get_groups(df, group_column, k)
#     
#     if (time_column == "OS_time") {
#         status_column <- "OS"
#     } else {
#         status_column <- "PFI_1"
#     }
#     
#     data.frame(
#         status = purrr::pluck(df, status_column), 
#         time = purrr::pluck(df, time_column),
#         variable = groups, 
#         measure = purrr::pluck(df, group_column)
#     ) %>% 
#         na.omit()
# }
# 
# 
# 
# get_concordance <- function(
#     df, value_column, time_column, status_column
# ) {
#     wrapr::let(
#         alias = c(valuevar = value_column,
#                   timevar = time_column,
#                   statusvar = status_column),
#         mat <- df %>% 
#             dplyr::select(valuevar, timevar, statusvar) %>% 
#             .[complete.cases(.),] %>% 
#             as.data.frame() %>% 
#             as.matrix()
#     )
#     
#     concordanceIndex::concordanceIndex(mat[,1], mat[,-1])
# }
# 
# get_concordance_by_group <- function(
#     df, value_columns, time_column, status_column
# ) {
#     value_columns %>% 
#         purrr::map(function(f) get_concordance(df, f, time_column, status_column)) %>% 
#         magrittr::set_names(value_columns)
# }
# 
# build_ci_mat <- function(
#     df, group_column, value_columns, time_column, status_column
# ) {
#     
#     value_names <- purrr::map(value_columns, get_variable_display_name)
#     group_v <- magrittr::extract2(df, group_column) 
#     groups <- group_v %>% 
#         unique() %>% 
#         purrr::discard(is.na(.)) %>% 
#         sort()
#     
#     df %>% 
#         split(group_v) %>% 
#         purrr::map(get_concordance_by_group, value_columns, time_column, status_column) %>% 
#         unlist() %>% 
#         unname() %>% 
#         matrix(ncol = length(groups)) %>%
#         magrittr::set_rownames(value_names) %>% 
#         magrittr::set_colnames(groups)
# }



###############################################################################
# Functions below this line have been deprecated
###############################################################################



# ** Driver correlation module ----

##
##  Builds data frame used for the regression 
##

# build_mutation_df <- function(df, response_var, group_column, group_options){
#     fmx_df.intermediate <- build_intermediate_fmx_df_for_groups(
#         df,
#         response_var,
#         group_column,
#         group_options)
#     driver_mutation_df.long <-
#         panimmune_data$driver_mutation_df %>%
#         tidyr::gather(key = "mutation", value = "value", -c("ParticipantBarcode")) %>%
#         dplyr::mutate(value = forcats::fct_relevel(value, "Wt", "Mut"))
#     mutation_df <- build_driver_mutation_df(driver_mutation_df.long, fmx_df.intermediate)
#     if(nrow(mutation_df) == 0){
#         mutation_df <- NULL
#     } else {
#         mutation_df <- label_driver_mutation_df(mutation_df, group_column)
#     }
#     return(mutation_df)
# }
# 
# 
# #
# # join driver data frame and fmx (feature matrix) data frame 
# #
# build_driver_mutation_df <- function(driver_df, fmx_df) {
#     driver_df %>%
#         dplyr::left_join(fmx_df, by="ParticipantBarcode") %>% 
#         tidyr::drop_na()
# }
# 
# label_driver_mutation_df <- function(df, group_column){
#     df_labeled <- wrapr::let(
#         c(gc = group_column),
#         dplyr::mutate(df, mutation_group = stringr::str_c(mutation, gc, sep = ".")))
# }
# 
# ## filter mutation data frame to mutations meeting a minimum overall count_threshold within a group
# ## count_threshold is the minimum mutation count required
# ## In rare cases, combinations where all samples, or all but one samples is mutations occur
# ## These are removed as well as significance testing cannot be performed
# 
# #
# # For each combination of mutation and group, compile total count and count mutated
# #
# build_mutation_group_summary_df <- function(df){
#     df %>%
#         dplyr::mutate(value = ifelse(value == "Wt", 0, 1)) %>% 
#         dplyr::select(mutation_group, value) %>%
#         dplyr::group_by(mutation_group) %>%
#         dplyr::summarise(
#             mutation_count = sum(value),
#             cat_count = dplyr::n()) %>%
#         dplyr::ungroup()
# }
# 
# #
# # identify which mutation groups combination have sufficient data for test
# #
# # uses a universal minimum count. Better might be to use percent of group size as a minimum.
# get_testable_mutation_groups <- function(df, count_threshold = 4){
#     df %>%
#         dplyr::filter(mutation_count >= count_threshold) %>% # requirement for a minimal mutation count
#         dplyr::filter(mutation_count < cat_count - 1) %>% # cannot test if all mutated or all but one mutated
#         magrittr::use_series(mutation_group)
# }
# 
# get_untestable_mutation_groups <- function(df, testable_mutation_groups){
#     df %>% 
#         dplyr::filter(!mutation_group %in% testable_mutation_groups) %>% 
#         magrittr::use_series(mutation_group)
# }
# 
# ##
# ## restrict fmx_df to rows with available group values and a single selected value column
# ##
# build_intermediate_fmx_df_for_groups <- function(
#     df, value_column, group_column, group_options, id_column = "ParticipantBarcode" ) {
#     wrapr::let(
#         c(GROUP = group_column),
#         result_df <- df %>% 
#             dplyr::select(id_column, GROUP, value_column) %>% 
#             dplyr::filter(GROUP %in% group_options)) %>% .[complete.cases(.),]    
# }
# 
# ##
# ## filter mutation df to mutations meeting a minimum overall count_threshold
# ## currently not used, will need for "full pancan" mode (test before using)
# ##
# build_filtered_mutation_df_pancan <- function(df,count_threshold=80){   # select greater than 1% mutation for now
#   binvec <- c(0,1) ; names(binvec) <- c("Wt","Mut")
#   df.boole <- df  %>% 
#     mutate(boole=as.vector(binvec[.$value])) %>% 
#     dplyr::select(-value) %>% 
#     dplyr::rename(value=boole)
#   driver_mutation.mutcount <-  df.boole %>% 
#     dplyr::select(-ParticipantBarcode) %>% 
#     dplyr::group_by(mutation) %>%  dplyr::summarise(mutation_count = sum(value)) %>% ungroup()
#   drivers.keep <- driver_mutation.mutcount %>% 
#     dplyr::filter(mutation_count > count_threshold) %>%
#     .$mutation
#   df %>% dplyr::filter(mutation %in% drivers.keep)
# }
# 
# 
# 
# 
# 
# 
# ##
# ## Compute p values for each 'combo' of driver mutation and cohort
# ##
# compute_pvals_per_combo <- function(df, value_column, group_column){
#     result_vec <- wrapr::let(
#         c(response_var=value_column,
#             gc=group_column),
#         df %>% 
#             split(.$mutation_group) %>% 
#             purrr::map( ~ lm(response_var ~ value, data=.)) %>%
#             purrr::map(summary) %>%
#             purrr::map("coefficients") %>% 
#             purrr::map(~. ["valueMut","Pr(>|t|)"]) %>%
#             unlist()
#     )
#     
#     data.frame(
#         mutation_group=as.vector(names(result_vec)),
#         neglog_pval=as.vector(-log10(result_vec)),
#         stringsAsFactors = FALSE)
# }
# 
# ##
# ## Compute effect size  for each 'combo' of driver mutation and cohort
# ##
# 
# compute_effect_size_per_combo <- function(df, value_column, group_column){
#     wrapr::let(
#         c(response_var=value_column,gc=group_column),
#         df_means <- df %>% 
#             dplyr::group_by(mutation_group,value) %>%
#             dplyr::summarize(mean_response=mean(response_var)) %>%
#             tidyr::spread(value,mean_response) %>%
#             dplyr::mutate(effect_size=-log10(Wt/Mut)) %>%
#             dplyr::select(-c(Wt,Mut)) %>% 
#             as.data.frame
#     )
# }
# 
# 
# 
# 
# ##
# ## Compute p-value and effect size for each combo and combine to single data frame
# ##
# compute_driver_associations <- function(df_for_regression,response_var,group_column,group_options){
#     res1 <- compute_pvals_per_combo(df_for_regression,response_var, group_column)
#     res2 <- compute_effect_size_per_combo(df_for_regression,response_var, group_column)
#     dplyr::inner_join(res1,res2,by="mutation_group") ## returns df with combo,neglog_pval,effect_size
# }


# ** IO target module ----

# build_io_target_expr_plot_df <- function(df, filter_value, group_option) { # added Oct 24, 2018
#     result_df <- panimmune_data$io_target_expr_df %>%
#         dplyr::filter(Symbol == filter_value) %>%
#         dplyr::left_join(df) %>%
#         dplyr::mutate(log_count = log10(normalized_count + 1)) %>%
#         dplyr::select(x = group_option, y = log_count) %>% 
#         tidyr::drop_na()
# }


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

# immunomodulator functions ---------------------------------------------------

# build_immunomodulator_expression_df <- function(
#     group_df, filter_value, group_col,
#     expression_df = panimmune_data$im_expr_df,
#     expression_filter_col = "Symbol",
#     expression_col = "normalized_count",
#     id_col = "ParticipantBarcode"){
#     
#     expression_df <- filter_immunomodulator_expression_df(
#         expression_df, 
#         id_col, 
#         expression_filter_col, 
#         expression_col, 
#         filter_value)
#     
#     group_df <- group_df %>% 
#         get_complete_df_by_columns(c(group_col, id_col)) %>% 
#         select(GROUP = group_col, ID = id_col)
#     
#     
#     result_df <- 
#         dplyr::inner_join(group_df, expression_df, by = "ID") %>%
#         dplyr::select(GROUP, LOG_COUNT)
#     
# }
# 
# filter_immunomodulator_expression_df <- function(
#     df, id_col, filter_col, expression_col, filter_value){
#     
#     df %>% 
#         get_complete_df_by_columns(c(
#             id_col, 
#             filter_col, 
#             expression_col)) %>% 
#         dplyr::select(
#             FILTER = filter_col, 
#             COUNT = expression_col,
#             ID = id_col) %>% 
#         dplyr::filter(FILTER == filter_value) %>% 
#         dplyr::mutate(LOG_COUNT = log10(COUNT + 1)) %>% 
#         dplyr::select(LOG_COUNT, ID)
# }
# 
# 
# build_immunomodulator_violin_plot_df <- function(df){
#     df %>%
#         dplyr::select(x = GROUP, y = LOG_COUNT) %>% 
#         get_complete_df_by_columns(c("x", "y"))
# }
# 
# build_immunomodulator_histogram_df <- function(df, selected_group){
#     df %>%
#         filter(GROUP == selected_group) %>% 
#         select(x = LOG_COUNT) %>% 
#         tidyr::drop_na()
# }


# immunefeatures -------------------------------------------

# build_intermediate_corr_df <- function(
#     df, value1_column, group_column, group_options, value2_columns,
#     id_column = "ParticipantBarcode" ) {
#     
#     wrapr::let(
#         c(GROUP = group_column),
#         result_df <- df %>%
#             dplyr::select(id_column, GROUP, value1_column, value2_columns) %>%
#             dplyr::filter(GROUP %in% group_options))
# }

# build_immunefeatures_violin_plot_df <- function(df, x_col, y_col){
#   df %>%
#     dplyr::select(x = x_col, y = y_col) %>%
#     tidyr::drop_na()
# }

# 
# build_immunefeatures_df <- function(
#     df,
#     group_column,
#     value1_column,
#     value2_columns,
#     group_options,
#     id_column = "ParticipantBarcode"){
#     
#     assert_df_has_columns(
#         df, c(group_column, value1_column, value2_columns, id_column))
#     
#     result_df <- df %>%
#         dplyr::select(
#             ID = id_column,
#             GROUP = group_column,
#             VALUE1 = value1_column,
#             value2_columns) %>%
#         dplyr::filter(GROUP %in% group_options) %>%
#         dplyr::filter(!is.na(VALUE1))
#     assert_df_has_columns(result_df, c("ID", "GROUP", "VALUE1", value2_columns))
#     return(result_df)
# }
# 
# build_immunefeatures_correlation_matrix <- function(df, method = "spearman") {
#     long_df  <- df %>%
#         dplyr::select(-ID) %>%
#         tidyr::gather(
#             key = "VARIABLE",
#             value = "VALUE2",
#             -c(GROUP, VALUE1)) %>%
#         dplyr::group_by(GROUP, VARIABLE) %>%
#         tidyr::drop_na()
#     
#     if(nrow(long_df) == 0) return(long_df)
#     
#     result_matrix <- long_df %>%
#         dplyr::summarise(COR = cor(
#             VALUE1,
#             VALUE2,
#             method = method,
#             use = "pairwise.complete.obs")) %>%
#         tidyr::spread(key = "GROUP", value = "COR", fill = NA) %>%
#         dplyr::mutate(VARIABLE = purrr::map(VARIABLE, get_variable_display_name)) %>%
#         as.data.frame() %>%
#         tibble::column_to_rownames("VARIABLE") %>%
#         as.matrix()
# }
# 
# 
# build_immunefeatures_scatter_plot_df <- function(df, x_col, group_filter_value){
#     assert_df_has_columns(df, c(x_col, "VALUE1", "ID", "GROUP"))
#     df %>%
#         select(ID, GROUP, y = "VALUE1", x = x_col) %>%
#         filter(GROUP == group_filter_value) %>%
#         create_label(
#             name_column = "ID",
#             group_column = "GROUP",
#             value_columns = c("x", "y")) %>%
#         select("x", "y", "label") %>%
#         get_complete_df_by_columns(c("x", "y", "label"))
# }


# ** Driver correlation module ----
# 
# ##
# ##  Builds data frame used for the regression 
# ##
# 
# build_mutation_df <- function(df, response_var, group_column, group_options){
#     fmx_df.intermediate <- build_intermediate_fmx_df_for_groups(
#         df,
#         response_var,
#         group_column,
#         group_options)
#     driver_mutation_df.long <-
#         panimmune_data$driver_mutation_df %>%
#         tidyr::gather(key = "mutation", value = "value", -c("ParticipantBarcode")) %>%
#         dplyr::mutate(value = forcats::fct_relevel(value, "Wt", "Mut"))
#     mutation_df <- build_driver_mutation_df(driver_mutation_df.long, fmx_df.intermediate)
#     if(nrow(mutation_df) == 0){
#         mutation_df <- NULL
#     } else {
#         mutation_df <- label_driver_mutation_df(mutation_df, group_column)
#     }
#     return(mutation_df)
# }
# 
# 
# #
# # join driver data frame and fmx (feature matrix) data frame 
# #
# build_driver_mutation_df <- function(driver_df, fmx_df) {
#     driver_df %>%
#         dplyr::left_join(fmx_df, by="ParticipantBarcode") %>% 
#         tidyr::drop_na()
# }
# 
# label_driver_mutation_df <- function(df, group_column){
#     df_labeled <- wrapr::let(
#         c(gc = group_column),
#         dplyr::mutate(df, mutation_group = stringr::str_c(mutation, gc, sep = ".")))
# }
# 
# ## filter mutation data frame to mutations meeting a minimum overall count_threshold within a group
# ## count_threshold is the minimum mutation count required
# ## In rare cases, combinations where all samples, or all but one samples is mutations occur
# ## These are removed as well as significance testing cannot be performed
# 
# #
# # For each combination of mutation and group, compile total count and count mutated
# #
# build_mutation_group_summary_df <- function(df){
#     df %>%
#         dplyr::mutate(value = ifelse(value == "Wt", 0, 1)) %>% 
#         dplyr::select(mutation_group, value) %>%
#         dplyr::group_by(mutation_group) %>%
#         dplyr::summarise(
#             mutation_count = sum(value),
#             cat_count = dplyr::n()) %>%
#         dplyr::ungroup()
# }
# 
# #
# # identify which mutation groups combination have sufficient data for test
# #
# # uses a universal minimum count. Better might be to use percent of group size as a minimum.
# get_testable_mutation_groups <- function(df, count_threshold = 4){
#     df %>%
#         dplyr::filter(mutation_count >= count_threshold) %>% # requirement for a minimal mutation count
#         dplyr::filter(mutation_count < cat_count - 1) %>% # cannot test if all mutated or all but one mutated
#         magrittr::use_series(mutation_group)
# }
# 
# get_untestable_mutation_groups <- function(df, testable_mutation_groups){
#     df %>% 
#         dplyr::filter(!mutation_group %in% testable_mutation_groups) %>% 
#         magrittr::use_series(mutation_group)
# }
# 
# ##
# ## restrict fmx_df to rows with available group values and a single selected value column
# ##
# build_intermediate_fmx_df_for_groups <- function(
#     df, value_column, group_column, group_options, id_column = "ParticipantBarcode" ) {
#     wrapr::let(
#         c(GROUP = group_column),
#         result_df <- df %>% 
#             dplyr::select(id_column, GROUP, value_column) %>% 
#             dplyr::filter(GROUP %in% group_options)) %>% .[complete.cases(.),]    
# }
# 
# ##
# ## filter mutation df to mutations meeting a minimum overall count_threshold
# ## currently not used, will need for "full pancan" mode (test before using)
# ##
# build_filtered_mutation_df_pancan <- function(df,count_threshold=80){   # select greater than 1% mutation for now
#     binvec <- c(0,1) ; names(binvec) <- c("Wt","Mut")
#     df.boole <- df  %>% 
#         mutate(boole=as.vector(binvec[.$value])) %>% 
#         dplyr::select(-value) %>% 
#         dplyr::rename(value=boole)
#     driver_mutation.mutcount <-  df.boole %>% 
#         dplyr::select(-ParticipantBarcode) %>% 
#         dplyr::group_by(mutation) %>%  dplyr::summarise(mutation_count = sum(value)) %>% ungroup()
#     drivers.keep <- driver_mutation.mutcount %>% 
#         dplyr::filter(mutation_count > count_threshold) %>%
#         .$mutation
#     df %>% dplyr::filter(mutation %in% drivers.keep)
# }
# 
# 
# 
# 
# 
# 
# ##
# ## Compute p values for each 'combo' of driver mutation and cohort
# ##
# compute_pvals_per_combo <- function(df, value_column, group_column){
#     result_vec <- wrapr::let(
#         c(response_var=value_column,
#           gc=group_column),
#         df %>% 
#             split(.$mutation_group) %>% 
#             purrr::map( ~ lm(response_var ~ value, data=.)) %>%
#             purrr::map(summary) %>%
#             purrr::map("coefficients") %>% 
#             purrr::map(~. ["valueMut","Pr(>|t|)"]) %>%
#             unlist()
#     )
#     
#     data.frame(
#         mutation_group=as.vector(names(result_vec)),
#         neglog_pval=as.vector(-log10(result_vec)),
#         stringsAsFactors = FALSE)
# }
# 
# ##
# ## Compute effect size  for each 'combo' of driver mutation and cohort
# ##
# 
# compute_effect_size_per_combo <- function(df, value_column, group_column){
#     wrapr::let(
#         c(response_var=value_column,gc=group_column),
#         df_means <- df %>% 
#             dplyr::group_by(mutation_group,value) %>%
#             dplyr::summarize(mean_response=mean(response_var)) %>%
#             tidyr::spread(value,mean_response) %>%
#             dplyr::mutate(effect_size=-log10(Wt/Mut)) %>%
#             dplyr::select(-c(Wt,Mut)) %>% 
#             as.data.frame
#     )
# }
# 
# 
# 
# 
# ##
# ## Compute p-value and effect size for each combo and combine to single data frame
# ##
# compute_driver_associations <- function(df_for_regression,response_var,group_column,group_options){
#     res1 <- compute_pvals_per_combo(df_for_regression,response_var, group_column)
#     res2 <- compute_effect_size_per_combo(df_for_regression,response_var, group_column)
#     dplyr::inner_join(res1,res2,by="mutation_group") ## returns df with combo,neglog_pval,effect_size
# }

# cellcontent functions -------------------------------------------------------

# build_cellcontent_df <- function(df, group_column){
#     assert_df_has_columns(df, c(group_column, "Stromal_Fraction", "leukocyte_fraction"))
#     long_df <- df %>% 
#         dplyr::select(
#             GROUP = group_column,
#             "Stromal_Fraction", 
#             "leukocyte_fraction") %>% 
#         tidyr::drop_na()
#     
#     if(nrow(long_df) == 0) return(long_df)
#     
#     result_df <- long_df %>% 
#         dplyr::mutate(Tumor_Fraction = 1 - Stromal_Fraction) %>% 
#         tidyr::gather(fraction_type, fraction, -GROUP)
#     assert_df_has_columns(result_df, c("GROUP", "fraction_type", "fraction"))
#     return(result_df)
# }
# 
# build_cell_fraction_df <- function(df, group_column, value_columns){
#     assert_df_has_columns(df, c(group_column, value_columns))
#     result_df <- df %>% 
#         dplyr::select(GROUP = group_column, value_columns) %>% 
#         tidyr::gather(fraction_type, fraction, -GROUP) %>% 
#         tidyr::drop_na()
#     assert_df_has_columns(result_df, c("GROUP", "fraction_type", "fraction"))
#     return(result_df)
# }
# 
# 
# build_cellcontent_barplot_df <- function(df, x_column, y_column) {
#     assert_df_has_columns(df, c("GROUP", "fraction_type", "fraction"))
#     result_df <- df %>%
#         summarise_df_at_column(
#             column = "fraction",
#             grouping_columns = c("GROUP", "fraction_type"),
#             function_names = c("mean", "se")) %>% 
#         create_label(
#             title = stringr::str_to_title(y_column),
#             name_column = x_column,
#             group_column = "GROUP",
#             value_columns = c("mean", "se")) %>% 
#         dplyr::select(
#             x = "GROUP",
#             y = "mean",
#             color = "fraction_type",
#             error = "se",
#             label)
#     assert_df_has_columns(result_df, c("x", "y", "label", "color", "error"))
#     assert_df_has_rows(result_df)
#     return(result_df)
# }
# 
# build_cellcontent_scatterplot_df <- function(
#     df, group_column, group_filter_value, 
#     id_column = "ParticipantBarcode",
#     x_column = "Stromal_Fraction",
#     y_column = "leukocyte_fraction") {
#     
#     result_df  <- df %>%
#         select(
#             GROUP = group_column, 
#             ID = id_column, 
#             x = x_column, 
#             y = y_column) %>%
#         filter(GROUP == group_filter_value) %>%
#         create_label(
#             name_column = "ID",
#             group_column = "GROUP",
#             value_columns = c("x", "y")) %>% 
#         select(x, y, "label")
#     assert_df_has_columns(result_df, c("x", "y", "label"))
#     assert_df_has_rows(result_df)
#     return(result_df)
# }
