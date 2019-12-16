cat(crayon::magenta("Importing feather files for samples."), fill = TRUE)
driver_mutations1 <- feather::read_feather("../data2/driver_mutations1.feather")
driver_mutations2 <- feather::read_feather("../data2/driver_mutations2.feather")
driver_mutations3 <- feather::read_feather("../data2/driver_mutations3.feather")
driver_mutations4 <- feather::read_feather("../data2/driver_mutations4.feather")
driver_mutations5 <- feather::read_feather("../data2/driver_mutations5.feather")
feature_values_long <- feather::read_feather("../data2/feature_values_long.feather")
immunomodulator_expr <- feather::read_feather("../data2/immunomodulator_expr.feather")
io_target_expr1 <- feather::read_feather("../data2/io_target_expr1.feather")
io_target_expr2 <- feather::read_feather("../data2/io_target_expr2.feather")
io_target_expr3 <- feather::read_feather("../data2/io_target_expr3.feather")
io_target_expr4 <- feather::read_feather("../data2/io_target_expr4.feather")
til_image_links <- feather::read_feather("../data2/til_image_links.feather")
cat(crayon::blue("Imported feather files for samples."), fill = TRUE)

# Combine all the sample data. Include the feature_values_long dataframe but
# ensure its "value" field remains distinct from the "value" field renamed to
# "rna_seq_expr".
cat(crayon::magenta("Combining all the sample data."), fill = TRUE)
all_samples <-
  dplyr::bind_rows(
    driver_mutations1,
    driver_mutations2,
    driver_mutations3,
    driver_mutations4,
    driver_mutations5,
    immunomodulator_expr,
    io_target_expr1,
    io_target_expr2,
    io_target_expr3,
    io_target_expr4
  ) %>%
  dplyr::rename_at("value", ~("rna_seq_expr")) %>%
  dplyr::bind_rows(feature_values_long) %>%
  dplyr::arrange(sample)
cat(crayon::blue("Combined all the sample data."), fill = TRUE)

# Clean up.
rm(driver_mutations1)
rm(driver_mutations2)
rm(driver_mutations3)
rm(driver_mutations4)
rm(driver_mutations5)
rm(feature_values_long)
rm(immunomodulator_expr)
rm(io_target_expr1)
rm(io_target_expr2)
rm(io_target_expr3)
rm(io_target_expr4)
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
table_written <- samples %>% .GlobalEnv$write_table_ts(.GlobalEnv$con, "samples", .)
samples <- RPostgres::dbReadTable(.GlobalEnv$con, "samples") %>% dplyr::as_tibble()
cat(crayon::blue("Built the samples table."), fill = TRUE)

# Remove the large til_image_links as we are done with it.
rm(til_image_links)
cat("Cleaned up.", fill = TRUE)
gc()

cat(crayon::magenta("Building samples_to_tags data."), fill = TRUE)
tags <- RPostgres::dbReadTable(.GlobalEnv$con, "tags") %>%
  dplyr::as_tibble() %>%
  dplyr::select(id:name)
tags_length <- length(tags$name)
tags <- tags %>% as.list
samples_to_tags <- dplyr::tibble() %>% tibble::add_column(sample_id = NA %>% as.integer, tag_id = NA %>% as.integer)
sample_set <- all_samples %>% dplyr::select(sample, TCGA_Study, TCGA_Subtype, Immune_Subtype)

purrr::pmap(tags, ~{
  current_id <- ..1
  current_tag_name <- ..2
  row_number <- which(current_tag_name == tags$name)

  svMisc::progress(row_number, tags_length - 1, progress.bar = TRUE)

  sample_set <- sample_set %>%
    dplyr::select(sample, TCGA_Study, TCGA_Subtype, Immune_Subtype) %>%
    dplyr::distinct(sample, TCGA_Study, TCGA_Subtype, Immune_Subtype) %>%
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

cat("Cleaned up.", fill = TRUE)
gc()

cat(crayon::magenta("Building samples_to_tags table."), fill = TRUE)
table_written <- samples_to_tags %>% .GlobalEnv$write_table_ts(.GlobalEnv$con, "samples_to_tags", .)
cat(crayon::blue("Built samples_to_tags table."), fill = TRUE)

cat(crayon::magenta("Building samples_to_features data."), fill = TRUE)
features <- RPostgres::dbReadTable(.GlobalEnv$con, "features") %>%
  dplyr::as_tibble() %>%
  dplyr::select(id:name)
features_length <- length(features$name)
features <- features %>% as.list
features_to_samples <- dplyr::tibble() %>%
  tibble::add_column(
    feature_id = NA %>% as.integer,
    sample_id = NA %>% as.integer,
    value = NA %>% as.numeric,
    inf_value = NA %>% as.numeric
  )
sample_set <- all_samples %>% dplyr::select(sample, feature, value)

purrr::pmap(features, ~{
  current_id <- ..1
  current_feature_name <- ..2
  row_number <- which(current_feature_name == features$name)

  svMisc::progress(row_number, features_length - 1, progress.bar = TRUE)

  sample_set <- sample_set %>%
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

cat("Cleaned up.", fill = TRUE)
gc()

cat(crayon::magenta("Building features_to_samples table\n(Please be patient, this may take a little while as there are many rows to write.)"), fill = TRUE)
table_written <- features_to_samples %>% .GlobalEnv$write_table_ts(.GlobalEnv$con, "features_to_samples", .)
cat(crayon::blue("Built features_to_samples table."), fill = TRUE)

# cat(crayon::magenta("Building genes_to_samples."), fill = TRUE)
# genes <- RPostgres::dbReadTable(.GlobalEnv$con, "genes") %>%
#   dplyr::as_tibble() %>%
#   dplyr::select(id:hgnc)
# genes_length <- length(genes$hgnc)
# genes <- genes %>% as.list
# genes_to_samples <- dplyr::tibble() %>%
#   tibble::add_column(
#     gene_id = NA %>% as.integer,
#     sample_id = NA %>% as.integer,
#     status = NA %>% as.character,
#     rna_seq_expr = NA %>% as.numeric
#   )
# sample_set <- all_samples %>% dplyr::select(sample, gene, status, rna_seq_expr)
# 
# purrr::pmap(genes, ~{
#   current_id <- ..1
#   current_hgnc <- ..2
#   row_number <- which(current_id == genes$id)
# 
#   svMisc::progress(row_number, genes_length - 1, progress.bar = TRUE)
# 
#   sample_set <- sample_set %>%
#     dplyr::filter(gene == current_hgnc) %>%
#     dplyr::distinct(sample, status, rna_seq_expr)
# 
#   genes_to_samples <<- genes_to_samples %>% .GlobalEnv$rebuild_genes_to_samples(current_id, sample_set, genes)
# 
#   if (row_number == genes_length) {
#     cat(crayon::blue("Built genes_to_samples."), fill = TRUE)
#   }
# 
#   rm(current_id)
#   rm(current_hgnc)
#   rm(row_number)
#   rm(sample_set)
# })
# 
# cat("Cleaned up.", fill = TRUE)
# gc()
# 
# cat(crayon::magenta("Building genes_to_samples table.\n(These are two large datasets, please be patient as they are rebuilt.)"), fill = TRUE)
# table_written <- genes_to_samples %>% .GlobalEnv$write_table_ts(.GlobalEnv$con, "genes_to_samples", .)
# cat(crayon::blue("Built genes_to_samples table."), fill = TRUE)

# Remove the data we are done with.
rm(features_to_samples)
rm(features)
rm(features_length)
# rm(genes)
# rm(genes_length)
# rm(genes_to_samples)
rm(tags)
rm(tags_length)
rm(table_written)
rm(all_samples)
rm(sample_set)
rm(samples)
rm(samples_to_tags)
cat("Cleaned up.", fill = TRUE)
gc()
