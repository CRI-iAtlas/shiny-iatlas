create_db <- function(panimmune_data){
    con  <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")
    
    features <- panimmune_data$feature_df %>% 
        dplyr::filter(VariableType == "Numeric") %>%
        dplyr::select(
            feature = FeatureMatrixLabelTSV,
            display = FriendlyLabel,
            class   = `Variable Class`,
            order   = `Variable Class Order`
        )
    
    dplyr::copy_to(con, features, "features", temporary = FALSE)
    
    feature_values_wide <- panimmune_data$fmx_df %>% 
        dplyr::select(
            sample = ParticipantBarcode, 
            Study,
            Subtype_Curated_Malta_Noushmehr_et_al,
            Subtype_Immune_Model_Based,
            features$feature
        )
    
    feature_values_long <- feature_values_wide %>% 
        tidyr::pivot_longer(
            -c(sample, Study, Subtype_Curated_Malta_Noushmehr_et_al, Subtype_Immune_Model_Based),
            names_to = "feature",
            values_to = "value") %>% 
        dplyr::filter(!is.na(value))
    
    dplyr::copy_to(con, feature_values_wide, "feature_values_wide", temporary = FALSE)
    dplyr::copy_to(con, feature_values_long, "feature_values_long", temporary = FALSE)
    
    groups <- panimmune_data$sample_group_df %>% 
        dplyr::select(
            group = FeatureValue,
            group_name = FeatureName,
            color = FeatureHex,
            parent_group = sample_group,
            characteristics = Characteristics
        )
    
    dplyr::copy_to(con, groups, "groups", temporary = FALSE)
    
    subtype_groups <- panimmune_data$sample_group_df %>% 
        dplyr::select(
            group = FeatureValue,
            subtype_group = `TCGA Studies`,
            subtype_group_display = FeatureDisplayName
        ) %>% 
        tidyr::drop_na()
    
    dplyr::copy_to(con, subtype_groups, "subtype_groups", temporary = FALSE)
    
    immunomodulator_expr <- panimmune_data$im_expr_df %>% 
        dplyr::rename(sample = ParticipantBarcode) %>% 
        tidyr::pivot_longer(
            names_to = "gene",
            values_to = "value",
            -sample
        ) %>% 
        dplyr::inner_join(dplyr::select(feature_values_wide, 1:4))
    
    dplyr::copy_to(con, immunomodulator_expr, "immunomodulator_expr", temporary = FALSE)
    
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
    
    dplyr::copy_to(con, immunomodulators, "immunomodulators", temporary = FALSE)
    
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
    
    dplyr::copy_to(con, til_image_links, "til_image_links", temporary = FALSE)
    
    io_target_expr <- panimmune_data$io_target_expr_df %>% 
        dplyr::rename(sample = ParticipantBarcode) %>% 
        tidyr::pivot_longer(
            names_to = "gene",
            values_to = "value",
            -sample
        ) %>% 
        dplyr::inner_join(dplyr::select(feature_values_wide, 1:4))
    
    dplyr::copy_to(con, io_target_expr, "io_target_expr", temporary = FALSE)
    
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
        
    
    dplyr::copy_to(con, io_targets, "io_targets", temporary = FALSE)
    
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
        )
    
    dplyr::copy_to(con, driver_results, "driver_results", temporary = FALSE)
    
    driver_mutations <- panimmune_data$driver_mutations_df %>% 
        dplyr::rename(sample = ParticipantBarcode) %>% 
        tidyr::pivot_longer(
            names_to = "gene",
            values_to = "status",
            -sample
        ) %>% 
        tidyr::drop_na() %>% 
        dplyr::mutate(status = dplyr::if_else(status == "Wt", F, T)) %>% 
        dplyr::inner_join(dplyr::select(feature_values_wide, 1:4))
    
    
    return(con)
    
    
    
}

