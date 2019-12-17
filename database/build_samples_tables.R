# Combine all the sample data. Include the feature_values_long dataframe but
# ensure its "value" field (from feature_values_long) remains distinct from
# the "value" field renamed to "rna_seq_expr".
cat(crayon::magenta("Importing feather files for samples and combining all the sample data."), fill = TRUE)
feature_values_long <- feather::read_feather("../data2/feature_values_long.feather")
til_image_links <- feather::read_feather("../data2/til_image_links.feather")
all_samples <- dplyr::bind_rows(
    feather::read_feather("../data2/driver_mutations1.feather"),
    feather::read_feather("../data2/driver_mutations2.feather"),
    feather::read_feather("../data2/driver_mutations3.feather"),
    feather::read_feather("../data2/driver_mutations4.feather"),
    feather::read_feather("../data2/driver_mutations5.feather"),
    feather::read_feather("../data2/io_target_expr1.feather"),
    feather::read_feather("../data2/io_target_expr2.feather"),
    feather::read_feather("../data2/io_target_expr3.feather"),
    feather::read_feather("../data2/io_target_expr4.feather"),
    feather::read_feather("../data2/immunomodulator_expr.feather"
  )) %>%
  dplyr::rename_at("value", ~("rna_seq_expr")) %>%
  dplyr::bind_rows(feature_values_long) %>%
  dplyr::arrange(sample)
cat(crayon::blue("Imported feather files for samples and combined all the sample data."), fill = TRUE)

# Clean up.
rm(feature_values_long)
cat("Cleaned up.", fill = TRUE)
gc()

# Build the samples table.
# Get only the sample names (no duplicates).
cat(crayon::magenta("Preparing samples data."), fill = TRUE)
samples <- all_samples %>% 
  dplyr::distinct(sample) %>%
  dplyr::rename_at("sample", ~("sample_id")) %>%
  merge(til_image_links, by.x = "sample_id", by.y = "sample", all = TRUE) %>%
  dplyr::arrange(sample_id) %>%
  dplyr::rename_at("link", ~("tissue_id")) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(tissue_id = stringi::stri_extract_first(tissue_id, regex = "[\\w]{4}-[\\w]{2}-[\\w]{4}-[\\w]{3}-[\\d]{2}-[\\w]{3}"))

cat(crayon::magenta("Building the samples table."), fill = TRUE)
table_written <- samples %>% .GlobalEnv$write_table_ts("samples")
samples <- .GlobalEnv$read_table("samples") %>% dplyr::as_tibble()
cat(crayon::blue("Built the samples table."), fill = TRUE)

# Remove the large til_image_links as we are done with it.
rm(til_image_links)
cat("Cleaned up.", fill = TRUE)
gc()

cat(crayon::magenta("Building samples_to_tags data."), fill = TRUE)
samples_to_tags <- dplyr::tibble() %>% tibble::add_column(sample_id = NA %>% as.integer, tag_id = NA %>% as.integer)
tags <- .GlobalEnv$read_table("tags") %>%
  dplyr::as_tibble() %>%
  dplyr::select(id, name)
tags_length <- length(tags$name)
tags <- tags %>% as.list
samples_to_tags_sample_set <- all_samples %>% dplyr::distinct(sample, TCGA_Study, TCGA_Subtype, Immune_Subtype)

tags %>% purrr::pmap(~{
  current_id <- ..1
  current_tag_name <- ..2
  row_number <- which(current_tag_name == tags$name)

  svMisc::progress(row_number, tags_length - 1, progress.bar = TRUE)

  sample_set <- samples_to_tags_sample_set %>%
    dplyr::filter(TCGA_Study == current_tag_name | TCGA_Subtype == current_tag_name | Immune_Subtype == current_tag_name)

  samples_to_tags <<- samples_to_tags %>%
    .GlobalEnv$rebuild_samples_to_tags(current_id, current_tag_name, sample_set, samples)

  if (row_number == tags_length) {
    cat(crayon::blue("Built samples_to_tags data."), fill = TRUE)
  }

  rm(current_id)
  rm(current_tag_name)
  rm(row_number)
  rm(sample_set)
})

rm(samples_to_tags_sample_set)
cat("Cleaned up.", fill = TRUE)
gc()

cat(crayon::magenta("Building samples_to_tags table."), fill = TRUE)
table_written <- samples_to_tags %>% .GlobalEnv$write_table_ts("samples_to_tags")
cat(crayon::blue("Built samples_to_tags table."), fill = TRUE)

cat(crayon::magenta("Building samples_to_features data."), fill = TRUE)
features_to_samples <- dplyr::tibble() %>%
  tibble::add_column(
    feature_id = NA %>% as.integer,
    sample_id = NA %>% as.integer,
    value = NA %>% as.numeric,
    inf_value = NA %>% as.numeric
  )
features <- .GlobalEnv$read_table("features") %>%
  dplyr::as_tibble() %>%
  dplyr::select(id, name)
features_length <- length(features$name)
features <- features %>% as.list
features_to_samples_sample_set <- all_samples %>% dplyr::select(sample, feature, value)

features %>% purrr::pmap(~{
  current_id <- ..1
  current_feature_name <- ..2
  row_number <- which(current_feature_name == features$name)

  svMisc::progress(row_number, features_length - 1, progress.bar = TRUE)

  sample_set <- features_to_samples_sample_set %>%
    dplyr::select(sample, feature, value) %>%
    dplyr::filter(feature == current_feature_name) %>%
    dplyr::distinct(sample, feature, .keep_all = TRUE)

  features_to_samples <<- features_to_samples %>%
    .GlobalEnv$rebuild_features_to_samples(current_id, sample_set, samples)

  if (row_number == features_length) {
    cat(crayon::blue("Built samples_to_features data."), fill = TRUE)
  }

  rm(current_id)
  rm(current_feature_name)
  rm(row_number)
  rm(sample_set)
})

rm(features_to_samples_sample_set)
cat("Cleaned up.", fill = TRUE)
gc()

cat(crayon::magenta("Building features_to_samples table\n(Please be patient, this may take a little while as there are many rows to write.)"), fill = TRUE)
table_written <- features_to_samples %>% .GlobalEnv$write_table_ts("features_to_samples")
cat(crayon::blue("Built features_to_samples table."), fill = TRUE)

cat(crayon::magenta("Building genes_to_samples data.\n(These are two large datasets, please be patient as they are rebuilt.)"), fill = TRUE)
genes_to_samples <- dplyr::tibble() %>%
  tibble::add_column(
    gene_id = NA %>% as.integer,
    sample_id = NA %>% as.integer,
    status = NA %>% as.character,
    rna_seq_expr = NA %>% as.numeric
  )
genes <- .GlobalEnv$read_table("genes") %>%
  dplyr::as_tibble() %>%
  dplyr::select(id, hgnc)
genes_length <- length(genes$hgnc)
genes <- genes %>% as.list
genes_to_samples_sample_set <- all_samples %>% dplyr::as_tibble() %>% dplyr::select(sample, gene, status, rna_seq_expr)

genes %>% purrr::pmap(~{
  current_id <- ..1
  current_hgnc <- ..2
  row_number <- which(current_id == genes$id)

  svMisc::progress(row_number, genes_length - 1, progress.bar = TRUE)

  sample_set <- genes_to_samples_sample_set %>%
    dplyr::filter(gene == current_hgnc) %>%
    dplyr::distinct(sample, status, rna_seq_expr)

  genes_to_samples <<- genes_to_samples %>% .GlobalEnv$rebuild_genes_to_samples(current_id, sample_set, samples)

  if (row_number == genes_length) {
    cat(crayon::blue("Built genes_to_samples data."), fill = TRUE)
  }

  rm(current_id)
  rm(current_hgnc)
  rm(row_number)
  rm(sample_set)
})

rm(genes_to_samples_sample_set)
cat("Cleaned up.", fill = TRUE)
gc()

cat(crayon::magenta("Building genes_to_samples table.\n(There are many rows to write, this may take a little while.)"), fill = TRUE)
table_written <- genes_to_samples %>% .GlobalEnv$write_table_ts("genes_to_samples")
cat(crayon::blue("Built genes_to_samples table."), fill = TRUE)

# Remove the data we are done with.
rm(features_to_samples)
rm(features)
rm(features_length)
rm(genes)
rm(genes_length)
rm(genes_to_samples)
rm(tags)
rm(tags_length)
rm(samples_to_tags)
rm(table_written)
rm(all_samples)
rm(sample_set)
rm(samples)
cat("Cleaned up.", fill = TRUE)
gc()
