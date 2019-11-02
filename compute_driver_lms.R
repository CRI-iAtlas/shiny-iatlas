library(magrittr)



data_df    <- feather::read_feather("data/fmx_df.feather")
driver_df  <- feather::read_feather("data/driver_mutations.feather")
feature_df <- feather::read_feather("data/feature_df.feather")
  

metrics <- feature_df %>%
    dplyr::filter(VariableType == "Numeric") %>%
    dplyr::pull(FeatureMatrixLabelTSV)

groups <- c("Study", "Subtype_Immune_Model_Based", "Subtype_Curated_Malta_Noushmehr_et_al")

genes <- driver_df %>% 
    dplyr::select(-ParticipantBarcode) %>% 
    colnames()

data_df <- data_df %>% 
    dplyr::inner_join(driver_df) %>% 
    dplyr::select(c(groups, metrics, genes))

rm(driver_df)

combination_df <-
    merge(metrics, genes) %>% 
    merge(data.frame(groups)) %>% 
    dplyr::rename(metric = x, gene = y, group = groups) %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate_if(is.factor, as.character) 

do_lm_by_groups <- function(metric, gene, group, data_df){
    data_df2 <- data_df %>% 
        dplyr::select(
            metric = metric,
            gene = gene,
            group = group
        ) %>% 
        tidyr::drop_na() %>% 
        dplyr::filter(!is.infinite(metric))%>%
        add_group_size_metrcs
    
    if (nrow(data_df2) == 0) return(dplyr::tibble())
    
    data_df2 %>% 
        add_effect_size_metrics %>% 
        tidyr::nest(metric, gene) %>% 
        dplyr::mutate(pvalue = purrr::map_dbl(data, calculate_lm_pvalue)) %>% 
        dplyr::mutate(neg_log10_pvalue = -log10(pvalue)) %>% 
        dplyr::select(-data) %>% 
        dplyr::mutate(label = stringr::str_c(gene, ";", group))
}

add_group_size_metrcs <- function(.tbl){
    .tbl %>% 
        dplyr::group_by(group) %>%
        dplyr::mutate(
            n_total = dplyr::n(),
            n_wt = sum(gene == "Wt")
        ) %>%
        dplyr::mutate(n_mut = n_total - n_wt) %>%
        dplyr::filter(n_mut > 1 & n_wt > 1) %>%
        dplyr::ungroup()
}

add_effect_size_metrics <- function(.tbl){
    effect_size_df <- .tbl %>% 
        dplyr::group_by(group, gene) %>% 
        dplyr::summarise(
            mean = mean(metric), 
            median = median(metric),
            sd = sd(metric)) %>% 
        dplyr::ungroup() %>% 
        tidyr::gather(
            key = summary_metric, 
            value = summary_value, 
            mean,
            median, 
            sd) %>% 
        tidyr::unite(col = summary_metric, gene, summary_metric) %>% 
        tidyr::spread(summary_metric, summary_value) %>% 
        dplyr::filter(Mut_mean > 0, Wt_mean > 0) %>% 
        dplyr::mutate(effect_size = -log10(Wt_mean/Mut_mean))
    
    dplyr::inner_join(.tbl, effect_size_df, by = "group")
    
}

calculate_lm_pvalue <- function(data){
    data %>% 
        lm(formula = metric ~ gene, data = .) %>% 
        summary %>% 
        magrittr::use_series(coefficients) %>% 
        .["geneWt", "Pr(>|t|)"]
}

result_df <- combination_df %>% 
    dplyr::mutate(res_df = purrr::pmap(., do_lm_by_groups, data_df)) %>% 
    tidyr::unnest()

feather::write_feather(result_df, "data2/driver_results.feather")
