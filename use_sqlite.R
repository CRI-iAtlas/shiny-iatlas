library(magrittr)

con  <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")

sample_v <- stringr::str_c("S", 1:20)
gene_v   <- stringr::str_c("gene", 1:200)

features <- dplyr::tibble(
    sample  = sample_v,
    group1  = stringr::str_c("C", sample(1:3, 20, replace = T)),
    group2  = stringr::str_c("D", sample(1:3, 20, replace = T)),
    metric1 = rpois(20, 10),
    metric2 = rpois(20, 10),
    metric3 = rpois(20, 10),
    cell1   = runif(20, 0, .3),
    cell2   = runif(20, 0, .2),
    cell3   = runif(20, 0, .1),
    cell4   = 1 - (cell3 + cell2 + cell1),
    cell5   = cell3 + cell2 + cell1,
    cell6   = cell5,
    fraction1 = runif(20, 0, .3),
    fraction2 = runif(20, 0, .4),
    fraction3 = 1 - (fraction1 + fraction2)
)

dplyr::copy_to(con, features, "features", temporary = FALSE)

features_long <- features %>% 
    tidyr::pivot_longer(-c(sample, group1, group2), names_to = "feature")

dplyr::copy_to(con, features_long, "features_long", temporary = FALSE)

samples <- dplyr::tibble(
    sample  = sample_v,
    gender  = sample(c("M", "F"), 20, replace = T),
    age     = sample(21:70, 20, replace = T)
)

dplyr::copy_to(con, samples, "samples", temporary = FALSE)

groups <- features %>% 
    dplyr::select(dplyr::starts_with("group")) %>% 
    tidyr::pivot_longer(
        dplyr::everything(), 
        names_to = "group_name",
        values_to = "group_value"
    ) %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(color = sample(colors(), dplyr::n()))


dplyr::copy_to(con, groups, "groups", temporary = FALSE)

metrics <- dplyr::tribble(
    ~name,       ~display, ~group,         ~member_order, ~color,    ~unit,       ~group2,
    "metric1",   "M1",     "group1",       NA,            NA,        "Count",     NA,
    "metric2",   "M2",     "group1",       NA,            NA,        "Count",     NA,
    "metric3",   "M3",     "group1",       NA,            NA,        "Count",     NA,
    "cell1",     "C1",     "cell_group1",  1,             "red",     "Fraction",  "deconvolution",
    "cell2",     "C2",     "cell_group1",  2,             "orange",  "Fraction",  "deconvolution",
    "cell3",     "C3",     "cell_group1",  3,             "yellow",  "Fraction",  "deconvolution",
    "cell4",     "C4",     "cell_group1",  4,             "blue",    "Fraction",  "deconvolution",
    "cell5",     "C5",     "cell_group2",  1,             "red",     "Fraction",  "deconvolution",
    "cell6",     "C6",     "cell_group2",  2,             "blue",    "Fraction",  "deconvolution",
    "fraction1", "F1",     "fractions",    1,             "yellow",  "Fraction",  NA,
    "fraction2", "F2",     "fractions",    2,             "green",   "Fraction",  NA,
    "fraction3", "F3",     "fractions",    3,             "purple",  "Fraction",  NA,
    "other1"   , "O1",     NA,             NA,            NA,        NA,          NA,
    "other2"   , NA,       NA,             NA,            NA,        NA,          NA,
 )

dplyr::copy_to(con, metrics, "metrics", temporary = FALSE)

expression <- 
    merge(sample_v, gene_v) %>% 
    dplyr::as_tibble() %>% 
    dplyr::rename(sample = x, gene = y) %>% 
    dplyr::mutate_all(as.character) %>% 
    dplyr::mutate(count = rpois(nrow(features) * 200, 10)) %>%
    dplyr::inner_join(dplyr::select(features, sample, group1, group2))

dplyr::copy_to(con, expression, "expression", temporary = FALSE)


genes <- dplyr::tibble(
    gene    = gene_v,
    group1  = str_c("G", sample(1:3, 200, replace = T), "A"),
    group2  = str_c("G", sample(1:3, 200, replace = T), "B")
)

dplyr::copy_to(con, genes, "genes", temporary = FALSE)

driver_mutation_status <-     
    merge(sample_v, gene_v) %>% 
    dplyr::as_tibble() %>% 
    dplyr::rename(sample = x, gene = y) %>% 
    dplyr::mutate_all(as.character) %>% 
    dplyr::mutate(mutated = sample(c(T, F), 4000, replace = T))

dplyr::copy_to(con, driver_mutation_status, "driver_mutation_status", temporary = FALSE)

driver_mutations <- driver_mutation_status %>% 
    dplyr::inner_join(features) %>% 
    dplyr::inner_join(samples)
    
dplyr::copy_to(con, driver_mutations, "driver_mutations", temporary = FALSE)

driver_results <- driver_mutation_status %>% 
    dplyr::inner_join(dplyr::select(features, sample, group1, group2)) %>% 
    tidyr::pivot_longer(
        c(group1, group2), 
        names_to = "group", 
        values_to = "group_value"
    ) %>% 
    dplyr::inner_join(dplyr::select(features_long, -c(group1, group2))) %>% 
    dplyr::group_by(gene, group_value, feature) %>% 
    dplyr::summarise(
        group_size = dplyr::n(), 
        group_mut  = sum(mutated),
        group_wt   = group_size - group_mut
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
        pvalue = runif(dplyr::n()),
        log10_pvalue = log10(pvalue),
        fold_change = abs(rnorm(dplyr::n(),5,5)) / abs(rnorm(dplyr::n(),5,5)),
        log10_fold_change = log10(fold_change),
        label = stringr::str_c(gene, group_value, sep = ";")
    )

dplyr::copy_to(con, driver_results, "driver_results", temporary = FALSE)

# immune features violin ----
# this will work for tilmaps

immune_feature_choices_list <- con %>% 
    dplyr::tbl("metrics") %>% 
    dplyr::select(group, name = display, value = name) %>% 
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(
        group = dplyr::if_else(
            is.na(group),
            "other",
            group
        ),
        name = dplyr::if_else(
            is.na(name),
            value,
            name
        )
    ) %>% 
    dplyr::as_tibble() %>% 
    tidyr::nest(data = c(name, value)) %>%
    dplyr::mutate(data = purrr::map(data, tibble::deframe)) %>%
    tibble::deframe()

group_choice   <- "group1"
feature_choice <- "metric1"
scaling_choice <- "log10+1"

    
    
immune_features_colors_con <- con %>% 
    dplyr::tbl("groups") %>%
    dplyr::filter(group_name == group_choice) %>% 
    dplyr::select(group = group_value,  color)
    

scale_value_column <- function(tbl, method){
    if(method == "log10+1"){
        return(dplyr::mutate(tbl, value = log(value + 1, 10)))
    } else if (method == "log10"){
        return(dplyr::mutate(tbl, value = log(value, 10)))
    } else if (method == "log2+1"){
        return(dplyr::mutate(tbl, value = log(value + 1, 2)))
    } else if(method == "log2"){
        return(dplyr::mutate(tbl, value = log(value, 2)))
    } else if(method == "none"){
        return(tbl)
    }
    stop("method not avaialble")
}

immune_feature_violin_con <- con %>% 
    dplyr::tbl("features_long") %>% 
    dplyr::select(group = group_choice, feature, value) %>% 
    dplyr::filter_all(dplyr::all_vars(!is.na(.))) %>% 
    dplyr::filter(feature == feature_choice) %>% 
    scale_value_column(scaling_choice) %>% 
    dplyr::inner_join(immune_features_colors_con, by = "group")

clicked_group <- "C1"

immune_feature_hist_con <- immune_feature_violin_con %>% 
    dplyr::filter(group == clicked_group) %>% 
    dplyr::select(value)
    

# immune features heatmap ----

immune_feature_choices_con <- con %>% 
    dplyr::tbl("metrics") %>% 
    dplyr::select(group, name = display, value = name) %>% 
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(
        group = dplyr::if_else(
            is.na(group),
            "other",
            group
        ),
        name = dplyr::if_else(
            is.na(name),
            value,
            name
        )
    ) 

immune_feature_choices_lst <- immune_feature_choices_con %>% 
    dplyr::as_tibble() %>% 
    tidyr::nest(data = c(name, value)) %>%
    dplyr::mutate(data = purrr::map(data, tibble::deframe)) %>%
    tibble::deframe()

group_choice        <- "group1"
feature_choice       <- "fraction1"
feature_group_choice <- "cell_group1"

feature_choices <- immune_feature_choices_con %>% 
    dplyr::filter(group == feature_group_choice) %>% 
    dplyr::pull(value)


immune_feature_con1 <- con %>% 
    dplyr::tbl("features_long") %>% 
    dplyr::filter(feature == feature_choice) %>% 
    dplyr::select(
        sample,
        group = group_choice, 
        value1 = value
    ) 
    
immune_feature_con2 <- con %>% 
    dplyr::tbl("features_long") %>% 
    dplyr::select(
        sample,
        feature,
        value2 = value
    ) %>% 
    dplyr::filter(feature %in% feature_choices)

immune_feature_heatmap_con <-
    dplyr::inner_join(
        immune_feature_con1,
        immune_feature_con2,
        by = "sample"
    ) %>% 
    dplyr::select(-sample) 


immune_feature_heatmap_matrix <- immune_feature_heatmap_con %>% 
    dplyr::as_tibble() %>% 
    dplyr::group_by(group, feature) %>%
    dplyr::summarise(value = cor(value1, value2)) %>% 
    tidyr::pivot_wider(names_from = group, values_from = value) %>% 
    tibble::column_to_rownames("feature") %>% 
    as.matrix()

clicked_group  <- "C3"
clicked_feature <- "cell2"

immune_feature_scatter_tbl <- immune_feature_heatmap_con %>% 
    dplyr::filter(
        group   == clicked_group,
        feature == clicked_feature
    ) %>% 
    dplyr::select(value1, value2)

# cellcontent 1 ----

group_choice <- "group2"

cell_content_colors_con <- con %>% 
    dplyr::tbl("metrics") %>%
    dplyr::filter(group == "fractions") %>% 
    dplyr::select(feature = name, color)

cell_content_con1 <- con %>% 
    dplyr::tbl("features_long") %>% 
    dplyr::select(sample, group = group_choice, feature, value) %>%
    dplyr::filter(feature %in% c("fraction1", "fraction2", "fraction3"))

cell_content_bar_tbl1 <- cell_content_con1 %>% 
    dplyr::group_by(group, feature) %>% 
    dplyr::summarise(
        mean = mean(value), 
        l = dplyr::n()
    ) %>%
    dplyr::mutate(se = mean / sqrt(l)) %>% 
    dplyr::select(-l) %>% 
    dplyr::inner_join(cell_content_colors_con, by = "feature") %>% 
    dplyr::as_tibble() 

clicked_group <- "D3"

cell_content_scatter_tbl <- cell_content_con1 %>% 
    dplyr::filter(
        group == clicked_group,
        feature %in% c("fraction1", "fraction2")
    ) %>% 
    dplyr::select(-group) %>% 
    dplyr::as_tibble() %>% 
    tidyr::pivot_wider(names_from = feature, values_from = value)
    

# cellcontent 2 ----
    
cell_content_group_con <- con %>% 
    dplyr::tbl("metrics") %>% 
    dplyr::filter(group2 == "deconvolution")

cell_content_group_choices <- cell_content_group_con %>% 
    dplyr::select(group) %>% 
    dplyr::distinct() %>% 
    dplyr::pull(group)

group_choice <- "group2"
cell_group_choice <- "cell_group1"

cell_content_colors_con2 <-  cell_content_group_con %>% 
    dplyr::filter(group == cell_group_choice) %>% 
    dplyr::select(feature = name, color)

cell_content_tbl2 <- con %>% 
    dplyr::tbl("features_long") %>% 
    dplyr::select(group = group_choice, feature, value) %>%
    dplyr::inner_join(cell_content_colors_con2, by = "feature") %>% 
    dplyr::group_by(group, feature) %>% 
    dplyr::summarise(mean = mean(value)) %>%
    dplyr::as_tibble() 

# immunomodulators ----
# This will work for drug targets
group_choice <- "group1"
im_group_choice <- "group2"

im_gene_choice_con <- con %>% 
    dplyr::tbl("genes") %>% 
    dplyr::select(gene, group = im_group_choice) %>% 
    dplyr::filter(!is.na(group)) 

im_gene_options <- im_gene_choice_con %>% 
    dplyr::select(gene) %>% 
    dplyr::distinct() %>% 
    dplyr::pull(gene)

im_gene_choice <- "gene1"

im_features_colors_con <- con %>% 
    dplyr::tbl("groups") %>%
    dplyr::filter(group_name == group_choice) %>% 
    dplyr::select(group = group_value,  color)

im_tbl <- con %>% 
    dplyr::tbl("expression") %>% 
    dplyr::select("sample", "gene", "count", group = group_choice) %>% 
    dplyr::filter(gene == im_gene_choice) %>% 
    dplyr::inner_join(im_features_colors_con, by = "group") %>% 
    dplyr::as_tibble()
    
    
    



    

# drivers module single variate ----


group_choice   <- "group1"
min_size       <- 5
min_wt         <- 2
min_mut        <- 2

driver_response_choices_tbl <- con %>% 
    dplyr::tbl("metrics") %>% 
    dplyr::select(name, display, group) %>% 
    dplyr::as_tibble()

selected_response <- "fraction3"

driver_volcano_tbl <- con %>% 
    dplyr::tbl("driver_results") %>% 
    dplyr::filter(
        group_size >= min_size,
        group_wt   >= min_wt,
        group_mut  >= min_mut,
        feature    == selected_response
    ) %>% 
    dplyr::select(label, log10_pvalue, log10_fold_change)

clicked_label <- "gene10;C1"

driver_violin_tbl <- con %>% 
    dplyr::tbl("driver_mutations") %>%
    dplyr::rename(group = group_choice, feature = selected_response) %>%
    dplyr::filter(paste(gene, group, sep = ";") == clicked_label) %>% 
    dplyr::select(mutated, feature) 

# driver module multi-variate -----

group_choice   <- "group1"
method_choice  <- "by_group"
min_size       <- 5
min_wt         <- 2
min_mut        <- 2

driver_response_choices_tbl <- con %>% 
    dplyr::tbl("metrics") %>% 
    dplyr::select(name, display, group) %>% 
    dplyr::as_tibble()

selected_response <- "fraction3"

driver_covariate_choices <-  con %>% 
    dplyr::tbl("driver_mutations") %>%
    dplyr::select(-c(sample, gene, mutated)) %>% 
    colnames()

selected_covariates <- c("metric1", "cell1", "age")

if(method_choice == "by_group"){
    driver_con <- con %>% 
        dplyr::tbl("driver_mutations") %>%
        dplyr::select(
            gene, 
            group = group_choice, 
            response = selected_response,
            mutated, 
            selected_covariates
        ) %>% 
        dplyr::group_by(gene, group) 
} else {
    driver_con <- con %>% 
        dplyr::tbl("driver_mutations") %>%
        dplyr::select(
            gene, 
            response = selected_response,
            mutated, 
            selected_covariates
        ) %>% 
        dplyr::group_by(gene) 
}

driver_con2 <- driver_con %>% 
    dplyr::mutate(group_size = dplyr::n(), group_mut  = sum(mutated)) %>% 
    dplyr::mutate(group_wt   = group_size - group_mut) %>% 
    dplyr::filter(
        group_size >= min_size,
        group_wt   >= min_wt,
        group_mut  >= min_mut
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-c(group_size, group_wt, group_mut))

if(method_choice == "by_group"){
    driver_con3 <- driver_con2 %>% 
        dplyr::mutate(label = paste(gene, group, sep = ";")) %>% 
        dplyr::select(-c(gene, group))
} else {
    driver_con3 <- driver_con2 %>% 
        dplyr::rename(label = gene)
}

clicked_label <- "gene10;C1"

driver_drilldown_tbl <- driver_con3 %>% 
    dplyr::filter(label == clicked_label) %>% 
    dplyr::select(response, mutated) %>% 
    dplyr::mutate(status = dplyr::if_else(
        mutated == 0,
        "WT",
        "Mut"
    )) %>% 
    dplyr::select(-mutated) %>% 
    dplyr::as_tibble()





