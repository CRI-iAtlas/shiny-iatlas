setwd("../")
config_yaml <- yaml::read_yaml("configuration.yaml")
purrr::walk(config_yaml$libraries, library, character.only = T)
purrr::walk(config_yaml$function_files, source)
USE_REMOTE_BQ <- F
USE_REMOTE_GS <- F
source("functions/load_data.R")
panimmune_data <- load_data()

features <- panimmune_data$feature_df %>% 
    dplyr::inner_join(
        panimmune_data$feature_method_df, 
        by = c("Origin" = "Feature Origin")
    ) %>% 
    dplyr::filter(VariableType == "Numeric") %>%
    dplyr::select(
        feature = FeatureMatrixLabelTSV,
        display = FriendlyLabel,
        class   = `Variable Class.x`,
        order   = `Variable Class Order`,
        unit    = Unit,
        methods_tag = `Methods Tag`
    ) %>% 
    dplyr::bind_rows(dplyr::tibble(
        feature  = "Tumor_fraction",
        display  = "Tumor Fraction",
        class    = "Overall Proportion",
        order    = 4
    )) 

feather::write_feather(features, "data2/features.feather")

categories <- panimmune_data$feature_df %>% 
    dplyr::inner_join(
        panimmune_data$feature_method_df, 
        by = c("Origin" = "Feature Origin")
    ) %>% 
    dplyr::filter(VariableType != "Numeric") %>% 
    dplyr::select(
        category = FeatureMatrixLabelTSV,
        display = FriendlyLabel
    ) %>% 
    dplyr::filter(
        !display %in% c("Immune Subtype", "TCGA Subtype", "TCGA Study")
    )

feather::write_feather(categories, "data2/categories.feather")

category_values_wide <- panimmune_data$fmx_df %>% 
    dplyr::select(
        sample = ParticipantBarcode, 
        TCGA_Study = Study,
        TCGA_Subtype = Subtype_Curated_Malta_Noushmehr_et_al,
        Immune_Subtype = Subtype_Immune_Model_Based,
        categories$category
    ) 

feather::write_feather(
    category_values_wide, 
    "data2/category_values_wide.feather"
)

category_values_wide %>% 
    tidyr::pivot_longer(
        -c(sample, TCGA_Study, Immune_Subtype, TCGA_Subtype),
        names_to = "category",
        values_to = "value"
    ) %>% 
    dplyr::filter(!is.na(value)) %>% 
    feather::write_feather("data2/category_values_long.feather")

feature_values_wide <- panimmune_data$fmx_df %>% 
    dplyr::mutate(Tumor_fraction = 1 - Stromal_Fraction) %>% 
    dplyr::select(
        sample = ParticipantBarcode, 
        TCGA_Study = Study,
        TCGA_Subtype = Subtype_Curated_Malta_Noushmehr_et_al,
        Immune_Subtype = Subtype_Immune_Model_Based,
        features$feature
    )

feather::write_feather(feature_values_wide, "data2/feature_values_wide.feather")

feature_values_wide %>% 
    tidyr::pivot_longer(
        -c(sample, TCGA_Study, Immune_Subtype, TCGA_Subtype),
        names_to = "feature",
        values_to = "value") %>% 
    dplyr::filter(!is.na(value)) %>% 
    feather::write_feather("data2/feature_values_long.feather")


groups <- panimmune_data$sample_group_df %>% 
    dplyr::select(
        group = FeatureValue,
        group_name = FeatureName,
        color = FeatureHex,
        parent_group = sample_group,
        characteristics = Characteristics,
        subtype_group = `TCGA Studies`,
        subtype_group_display = FeatureDisplayName
    ) %>% 
    dplyr::mutate(parent_group = dplyr::if_else(
        parent_group == "Study",
        "TCGA_Study",
        dplyr::if_else(
            parent_group == "Subtype_Immune_Model_Based",
            "Immune_Subtype", 
            "TCGA_Subtype"
        )
    )) %>% 
    dplyr::mutate(parent_group_display = stringr::str_replace_all(
        parent_group, 
        "_",
        " "
    ))

feather::write_feather(groups, "data2/groups.feather")

group_tbl1 <- dplyr::tribble(
    ~id,  ~name,                                       ~display,                                    ~parent,  ~color,
    1,    "dataset",                                   "Dataset",                                   NA,       NA,
    2,    "tcga",                                      "TCGA",                                      1,        "red",
    3,    "pcawg",                                     "PCAWG",                                     1,        "blue",
    4,    "gender",                                    "Gender",                                    NA,       NA,
    5,    "male",                                      "Male",                                      4,        "red",
    6,    "female",                                    "Female",                                    4,        "blue",
    7,    "race",                                      "Race",                                      NA,       NA,
    8,    "white",                                     "White",                                     7,        "red",      
    9,    "black_or_african_american",                 "Black or African American",                 7,        "yellow",    
    10,   "asian",                                     "Asian",                                     7,        "blue",                  
    11,   "american_indian_or_alaskan_native",         "American Indian or Alaskan Native",         7,        "green",
    12,   "native_hawaiian_or_other_pacific_islander", "Native Hawaiian or other Pacific Islander", 7,        "purple", 
    13,   "ethncity",                                  "Ethnicity",                                 NA,       NA,
    14,   "not_hispanic_or_latino",                    "Not Hispanic or Latino",                    13,       "red",
    15,   "hispanic_or_latino",                        "Hispanic or Latino",                        13,       "blue",
    16,   "Immune_Subtype",                            "Immune Subtype",                            NA,       NA,     
    17,   "TCGA_Subtype",                              "TCGA Subtype",                              NA,       NA,     
    18,   "TCGA_Study",                                "TCGA Study",                                NA,       NA
)
    
group_tbl2 <- groups %>% 
    dplyr::select(
        display = subtype_group_display,
        name = subtype_group
    ) %>% 
    dplyr::distinct() %>% 
    tidyr::drop_na() %>% 
    dplyr::mutate(
        parent = 17,
        id = 19:(19 + dplyr::n() - 1)
    )

group_tbl3 <- groups %>% 
    dplyr::select(
        display = group_name,
        name = group,
        color, 
        parent_group,
        subtype_group
    ) %>% 
    dplyr::left_join(
        dplyr::select(group_tbl1, name, parent = id),
        by = c("parent_group" = "name")
    ) %>% 
    dplyr::left_join(
        dplyr::select(group_tbl2, name, subgroup = id),
        by = c("subtype_group" = "name")
    ) %>% 
    dplyr::select(-c(parent_group, subtype_group)) %>% 
    dplyr::mutate(id = 39:(39 + dplyr::n() - 1)
)
    
groups2 <- dplyr::bind_rows(group_tbl1, group_tbl2, group_tbl3)

feather::write_feather(groups2, "data2/groups2.feather")

immunomodulator_expr <- panimmune_data$im_expr_df %>% 
    dplyr::rename(sample = ParticipantBarcode) %>% 
    tidyr::pivot_longer(
        names_to = "gene",
        values_to = "value",
        -sample
    ) %>% 
    dplyr::inner_join(dplyr::select(feature_values_wide, 1:4))

feather::write_feather(immunomodulator_expr, "data2/immunomodulator_expr.feather")

immunomodulators <- panimmune_data$im_direct_relationships %>% 
    dplyr::select(
        display = Gene, 
        gene = `HGNC Symbol`, 
        display2 = `Friendly Name`,
        entrez = `Entrez ID`,
        gene_family = `Gene Family`,
        super_category = `Super Category`,
        immune_checkpoint = `Immune Checkpoint`,
        gene_function = Function,
        reference = `Reference(s) [PMID]`
    )

feather::write_feather(immunomodulators, "data2/immunomodulators.feather")

til_image_links <- panimmune_data$fmx_df %>% 
    dplyr::filter(!is.na(Slide)) %>%
    dplyr::mutate(
        link = stringr::str_c(
            "<a href=\"",
            "https://quip1.bmi.stonybrook.edu:443/camicroscope/osdCamicroscope.php?tissueId=",
            Slide,
            "\">",
            Slide,
            "</a>"
        )
    ) %>% 
    dplyr::select(
        sample = "ParticipantBarcode", 
        link
    ) 

feather::write_feather(til_image_links, "data2/til_image_links.feather")

io_target_expr <- panimmune_data$io_target_expr_df %>% 
    dplyr::rename(sample = ParticipantBarcode) %>% 
    tidyr::pivot_longer(
        names_to = "gene",
        values_to = "value",
        -sample
    ) %>% 
    dplyr::inner_join(dplyr::select(feature_values_wide, 1:4))

feather::write_feather(
    io_target_expr[1:1000000,], 
    "data2/io_target_expr1.feather"
)
feather::write_feather(
    io_target_expr[1000001:2000000,], 
    "data2/io_target_expr2.feather"
)
feather::write_feather(
    io_target_expr[2000001:3000000,], 
    "data2/io_target_expr3.feather"
)
feather::write_feather(
    io_target_expr[3000001:nrow(io_target_expr),], 
    "data2/io_target_expr4.feather"
)


io_targets <- panimmune_data$io_target_annotations %>% 
    dplyr::select(
        display = Gene, 
        gene = `HGNC Symbol`, 
        display2 = `Friendly Name`,
        entrez = `Entrez ID`,
        pathway = Pathway,
        therapy_type = `Therapy Type`,
        description = Description,
        url = IO_target_URL
    ) %>% 
    dplyr::mutate(
        link_gene = .$url %>% 
            stringr::str_split(";") %>% 
            purrr::map(rev) %>% 
            purrr::map_chr(1)
    ) %>% 
    dplyr::mutate(link = paste(
        "<a href=\"",
        url, 
        "\">",
        link_gene,
        "</a>", 
        sep = ""
    )) %>% 
    dplyr::select(-c(url, link_gene))

feather::write_feather(io_targets, "data2/io_targets.feather")

driver_results <- panimmune_data$driver_result_df %>% 
    dplyr::select(
        label,
        feature = metric, 
        group = group2,
        parent_group = group1,
        n_wt,
        n_mut,
        pvalue,
        fold_change,
        log10_pvalue,
        log10_fold_change
    ) %>% 
    dplyr::mutate(parent_group = dplyr::if_else(
        parent_group == "Study",
        "TCGA_Study",
        dplyr::if_else(
            parent_group == "Subtype_Immune_Model_Based",
            "Immune_Subtype", 
            "TCGA_Subtype"
        )
    ))


feather::write_feather(
    driver_results[1:700000,], 
    "data2/driver_results1.feather"
)

feather::write_feather(
    driver_results[700001:nrow(driver_results),], 
    "data2/driver_results2.feather"
)

driver_mutations <- panimmune_data$driver_mutations_df %>%
    dplyr::rename(sample = ParticipantBarcode) %>%
    tidyr::pivot_longer(
        names_to = "gene",
        values_to = "status",
        -sample
    ) %>%
    tidyr::drop_na() %>%
    dplyr::inner_join(dplyr::select(feature_values_wide, 1:4))

feather::write_feather(
    driver_mutations[1:1500000,], 
    "data2/driver_mutations1.feather"
)
feather::write_feather(
    driver_mutations[1500001:3000000,],
    "data2/driver_mutations2.feather"
)
feather::write_feather(
    driver_mutations[3000001:4500000,],
    "data2/driver_mutations3.feather"
)
feather::write_feather(
    driver_mutations[4500001:6000000,],
    "data2/driver_mutations4.feather"
)
feather::write_feather(
    driver_mutations[6000001:nrow(driver_mutations),],
    "data2/driver_mutations5.feather"
)

