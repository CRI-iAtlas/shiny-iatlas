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
    
    
    return(con)
    
    
    
}

