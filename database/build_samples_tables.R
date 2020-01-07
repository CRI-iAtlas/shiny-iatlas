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
cat(crayon::magenta("Building samples data."), fill = TRUE)
samples <- all_samples %>%
  dplyr::distinct(sample) %>%
  dplyr::rename_at("sample", ~("sample_id")) %>%
  merge(til_image_links, by.x = "sample_id", by.y = "sample", all = TRUE) %>%
  dplyr::arrange(sample_id) %>%
  dplyr::rename_at("link", ~("tissue_id")) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(tissue_id = stringi::stri_extract_first(tissue_id, regex = "[\\w]{4}-[\\w]{2}-[\\w]{4}-[\\w]{3}-[\\d]{2}-[\\w]{3}"))
cat(crayon::blue("Built samples data."), fill = TRUE)

cat(crayon::magenta("Building the samples table."), fill = TRUE)
table_written <- samples %>% .GlobalEnv$write_table_ts("samples")
samples <- .GlobalEnv$read_table("samples") %>% dplyr::as_tibble()
cat(crayon::blue("Built the samples table. (", nrow(samples), "rows )"), fill = TRUE, sep = " ")

# Remove the large til_image_links as we are done with it.
rm(til_image_links)
cat("Cleaned up.", fill = TRUE)
gc()

cat(crayon::magenta("Building samples_to_tags data."), fill = TRUE)
tags <- .GlobalEnv$read_table("tags") %>%
  dplyr::as_tibble() %>%
  dplyr::select(id, name)
sample_set_tcga_study <- all_samples %>%
  dplyr::distinct(sample, TCGA_Study) %>%
  dplyr::inner_join(tags, by = c("TCGA_Study" = "name")) %>%
  dplyr::rename_at("id", ~("tag_id")) %>%
  dplyr::distinct(sample, tag_id)
sample_set_tcga_subtype <- all_samples %>%
  dplyr::distinct(sample, TCGA_Subtype) %>%
  dplyr::inner_join(tags, by = c("TCGA_Subtype" = "name")) %>%
  dplyr::rename_at("id", ~("tag_id")) %>%
  dplyr::distinct(sample, tag_id)
sample_set_immune_subtype <- all_samples %>%
  dplyr::distinct(sample, Immune_Subtype) %>%
  dplyr::inner_join(tags, by = c("Immune_Subtype" = "name")) %>%
  dplyr::rename_at("id", ~("tag_id")) %>%
  dplyr::distinct(sample, tag_id)
samples_to_tags <- sample_set_tcga_study %>%
  dplyr::bind_rows(sample_set_tcga_subtype, sample_set_immune_subtype) %>%
  dplyr::inner_join(samples, by = c("sample" = "sample_id")) %>%
  dplyr::distinct(id, tag_id) %>%
  dplyr::rename_at("id", ~("sample_id"))
cat(crayon::blue("Built samples_to_tags data."), fill = TRUE)

rm(sample_set_tcga_study)
rm(sample_set_tcga_subtype)
rm(sample_set_immune_subtype)
rm(tags)
cat("Cleaned up.", fill = TRUE)
gc()

cat(crayon::magenta("Building samples_to_tags table."), fill = TRUE)
table_written <- samples_to_tags %>% .GlobalEnv$write_table_ts("samples_to_tags")
cat(crayon::blue("Built samples_to_tags table. (", nrow(samples_to_tags), "rows )"), fill = TRUE, sep = " ")

cat(crayon::magenta("Building samples_to_features data."), fill = TRUE)
features <- .GlobalEnv$read_table("features") %>%
  dplyr::as_tibble() %>%
  dplyr::select(id, name)
sample_set_features <- all_samples %>%
  dplyr::distinct(sample, feature, value) %>%
  dplyr::inner_join(features, by = c("feature" = "name")) %>%
  dplyr::rename_at("id", ~("feature_id")) %>%
  dplyr::distinct(sample, feature_id, value)
features_to_samples <- sample_set_features %>%
  dplyr::inner_join(samples, by = c("sample" = "sample_id")) %>%
  dplyr::distinct(id, feature_id, value) %>%
  dplyr::rename_at("id", ~("sample_id")) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(inf_value = ifelse(is.infinite(value), value, NA), value = ifelse(is.finite(value), value, NA))
cat(crayon::blue("Built samples_to_features data."), fill = TRUE)

rm(sample_set_features)
rm(features)
cat("Cleaned up.", fill = TRUE)
gc()

cat(crayon::magenta("Building features_to_samples table\n(Please be patient, this may take a little while as there are", nrow(features_to_samples), "rows to write.)"), fill = TRUE, sep = " ")
table_written <- features_to_samples %>% .GlobalEnv$write_table_ts("features_to_samples")
cat(crayon::blue("Built features_to_samples table. (", nrow(features_to_samples), "rows )"), fill = TRUE, sep = " ")

cat(crayon::magenta("Building genes_to_samples data.\n(These are two large datasets, please be patient as they are rebuilt.)"), fill = TRUE)
genes <- .GlobalEnv$read_table("genes") %>%
  dplyr::as_tibble() %>%
  dplyr::select(id, hgnc)
genes_to_samples <- all_samples %>%
  dplyr::distinct(sample, gene, status, rna_seq_expr) %>%
  dplyr::inner_join(genes, by = c("gene" = "hgnc")) %>%
  dplyr::rename_at("id", ~("gene_id")) %>%
  dplyr::distinct(sample, gene_id, status, rna_seq_expr) %>%
  dplyr::inner_join(samples, by = c("sample" = "sample_id")) %>%
  dplyr::distinct(id, gene_id, status, rna_seq_expr) %>%
  dplyr::rename_at("id", ~("sample_id")) %>%
  dplyr::arrange(sample_id, gene_id, status, rna_seq_expr) %>%
  dplyr::group_by(sample_id, gene_id) %>%
  dplyr::summarise(
    status = .GlobalEnv$filter_na(status),
    rna_seq_expr = .GlobalEnv$filter_na(rna_seq_expr)
  )
cat(crayon::blue("Built genes_to_samples data."), fill = TRUE)

cat(crayon::magenta("Building genes_to_samples table.\n(There are", nrow(genes_to_samples), "rows to write, this may take a little while.)"), fill = TRUE)
table_written <- genes_to_samples %>% .GlobalEnv$write_table_ts("genes_to_samples")
cat(crayon::blue("Built genes_to_samples table. (", nrow(genes_to_samples), "rows )"), fill = TRUE, sep = " ")

# Remove the data we are done with.
rm(features_to_samples)
rm(genes)
rm(genes_to_samples)
rm(samples_to_tags)
rm(table_written)
rm(all_samples)
rm(samples)
cat("Cleaned up.", fill = TRUE)
gc()
