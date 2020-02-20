# Load curated data ----

load_manifest <- function() {
    if (!USE_REMOTE_GS) {
        list(
            feature_df = feather::read_feather("data/feature_df.feather"),
            feature_method_df = feather::read_feather("data/feature_method_df.feather"),
            sample_group_df = feather::read_feather("data/sample_group_df.feather")
        )
    } else {
        fetch_manifest() %>% 
            format_manifest()
    }
}

load_feature_matrix <- function() {
    if (!USE_REMOTE_GS) {
        fmx <- list(
            fmx_df = feather::read_feather("data/fmx_df.feather")
        )
    } else {
        fmx <- fetch_feature_matrix() %>% 
            format_feature_matrix()
    }
    return(fmx)
}

load_im_annotations <- function() {
    if (!USE_REMOTE_GS) {
        list(
            im_direct_relationships = feather::read_feather(
                "data/im_direct_relationships.feather"
            ),
            im_potential_factors = feather::read_feather(
                "data/im_potential_factors.feather"
            )
        )
    } else {
        fetch_im_annotations() %>% 
            format_im_annotations()
    }
}

load_io_target_annotations <- function() {
  if (!USE_REMOTE_GS) {
    list(
      io_target_annotations = feather::read_feather(
        "data/io_target_annotations.feather"
      )
#      im_potential_factors = feather::read_feather(
#        "data/im_potential_factors.feather"
#      )
    )
  } else {
    fetch_im_annotations() %>% #### needs updating 
      format_im_annotations() #### needs updating 
  }
}


load_im_expression <- function() {
    if (!USE_REMOTE_BQ) {
        list(
            im_expr_df = feather::read_feather("data/im_expr_df.feather") %>% 
              dplyr::group_by(ParticipantBarcode, Symbol) %>% 
              dplyr::summarise(normalized_count = mean(normalized_count)) %>% 
              dplyr::ungroup() %>% 
              tidyr::spread(key = Symbol, value = normalized_count)
        )
    } else {
        fetch_im_expression() %>% 
            format_im_expression()
    }
}

load_io_target_expression <- function() {
  if (!USE_REMOTE_BQ) {
    list(
      io_target_expr_df = feather::read_feather("data/io_target_expr_df.feather") 
    )
  } else {
    fetch_im_expression() %>% #### needs updating 
      format_im_expression() #### needs updating 
  }
}


load_driver_mutation <- function() {
  if (!USE_REMOTE_BQ) {
    list(
      driver_mutation_df = feather::read_feather("data/driver_mutations.feather")
    )
  } else {
    fetch_driver_mutation() %>% 
      format_driver_mutation()
  }
}

load_extracellular_network <- function(){
  if (!USE_REMOTE_BQ) {
    list(
      all_net_info = list(
        "immune"= list("upbin_ratio" = feather::read_feather("data/network/nodes_TCGAImmune.feather"), "edges_score" = feather::read_feather("data/network/edges_TCGAImmune.feather")),
        "subtype"= list("upbin_ratio" = feather::read_feather("data/network/nodes_TCGASubtype.feather"), "edges_score" = feather::read_feather("data/network/edges_TCGASubtype.feather")),
        "study"= list("upbin_ratio" = feather::read_feather("data/network/nodes_TCGAStudy.feather"), "edges_score" = feather::read_feather("data/network/edges_TCGAStudy.feather")),
        "studyImmune" = list("upbin_ratio" = feather::read_feather("data/network/nodes_TCGAStudy_Immune.feather"), "edges_score" = feather::read_feather("data/network/edges_TCGAStudy_Immune.feather"))
      ),
      dfe_in = feather::read_feather("data/network/expr_data_merged.feather"),
      node_type = feather::read_feather("data/network/network_node_label_friendly.feather"),
      cell_scaffold = feather::read_feather("data/network/scaffold_network_cellimage.feather"),
      cell_coordinate = feather::read_feather("data/network/nodes_position_cell_image.feather")
    )
  } #else {
  #   fetch_driver_mutation() %>%
  #     format_driver_mutation()
  # }
  
}

## selection choices for the cell fractions.  Lots of other choices possible.
create_cell_fraction_options <- function() {
    if (!USE_REMOTE_BQ) {
        config_yaml$cell_fractions_local
    } else {
        config_yaml$cell_fractions_bq
    }
}

# Load global data ----

load_data <- function() {
    
    manifest_data <- load_manifest()
    feature_matrix_data <- load_feature_matrix()
    im_annotations_data <- load_im_annotations()
    im_expression_data <- load_im_expression()
    io_target_annotations_data <- load_io_target_annotations()
    io_target_expression_data <- load_io_target_expression()
    driver_mutation_data <- load_driver_mutation()
    extracellular_network_data <- load_extracellular_network()
    list(
        feature_df = manifest_data$feature_df,
        feature_method_df = manifest_data$feature_method_df,
        sample_group_df = manifest_data$sample_group_df,
        fmx_df = feature_matrix_data$fmx_df,
        im_direct_relationships = im_annotations_data$im_direct_relationships,
        im_potential_factors = im_annotations_data$im_potential_factors,
        im_expr_df = im_expression_data$im_expr_df,
        io_target_annotations = io_target_annotations_data$io_target_annotations,
        io_target_expr_df = io_target_expression_data$io_target_expr_df,
        driver_mutation_df = driver_mutation_data$driver_mutation_df,
        ecn_df = extracellular_network_data$all_net_info,
        ecn_labels = extracellular_network_data$node_type,
        ecn_expr = extracellular_network_data$dfe_in,
        ci_scaffold = extracellular_network_data$cell_scaffold,
        ci_coord = extracellular_network_data$cell_coordinate
    )
}
