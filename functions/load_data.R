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

## selection choices for the cell fractions.  Lots of other choices possible.
create_cell_fraction_options <- function() {
    if (!USE_REMOTE_BQ) {
        config_yaml$cell_fractions_local
    } else {
        config_yaml$cell_fractions_bq
    }
}

#########################################################################
##
## Get cell image grid object and associated annotations
##
#########################################################################

load_cellimage_base <- function(){
  pic <- grImport2::readPicture("data/tcell-svg-take3-cairo.svg")
  pic <- grImport2::readPicture("../CellImage_Work/tcell-svg-take4-cairo.svg")
  image_grob <- grImport2::pictureGrob(pic)
  gTree_name <- grid::childNames(image_grob) ## label of overall gTree object
  pathlabels <- image_grob$children[[gTree_name]]$childrenOrder ## labels and order of children 

  ## Variable annotations are ImageVariableID, FeatureLabel, Source, ColorScale
  variable_annotations <- readr::read_tsv('data/cell_image_id_annotations.tsv') 
  ## Image obects, in order, labeled in terms of ImageVariableID
  image_object_labels <- read.table('data/cell_image_object_ids.txt',as.is=T)$V1
  missing_annotations <- setdiff(image_object_labels,variable_annotations$ImageVariableID)
  if (length(missing_annotations) > 0 ){
    stop("Image objects ",paste(missing_annotations,collapse=" ")," do not have an annotation.")
  }
  unique_image_variable_ids <- unique(image_object_labels)
  
  cellimage_parts <- list()
  cellimage_parts$image_grob <- image_grob
  cellimage_parts$pathlabels <- pathlabels
  cellimage_parts$gTree_name <- gTree_name
  cellimage_parts$variable_annotations <- variable_annotations
  cellimage_parts$image_object_labels <- image_object_labels
  cellimage_parts$unique_image_variable_ids <- unique_image_variable_ids
  list(cellimage_base=cellimage_parts)
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
    cell_image_base_data <- load_cellimage_base()
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
        cell_image_base =  cell_image_base_data$cellimage_base
    )
}
