# Get data from feather files as data.frames and convert them to a tibbles.
driver_mutations1 <-
  feather::read_feather("data2/driver_mutations1.feather")
driver_mutations2 <-
  feather::read_feather("data2/driver_mutations2.feather")
driver_mutations3 <-
  feather::read_feather("data2/driver_mutations3.feather")
driver_mutations4 <-
  feather::read_feather("data2/driver_mutations4.feather")
driver_mutations5 <-
  feather::read_feather("data2/driver_mutations5.feather")
feature_values_long <-
  feather::read_feather("data2/feature_values_long.feather")
immunomodulator_expr <-
  feather::read_feather("data2/immunomodulator_expr.feather")
io_target_expr1 <-
  feather::read_feather("data2/io_target_expr1.feather")
io_target_expr2 <-
  feather::read_feather("data2/io_target_expr2.feather")
io_target_expr3 <-
  feather::read_feather("data2/io_target_expr3.feather")
io_target_expr4 <-
  feather::read_feather("data2/io_target_expr4.feather")
til_image_links <-
  feather::read_feather("data2/til_image_links.feather")

cat("Imported feather files for samples.", fill = TRUE)

# Combine all the sample data.
all_samples <-
  dplyr::bind_rows(
    driver_mutations1,
    driver_mutations2,
    driver_mutations3,
    driver_mutations4,
    driver_mutations5,
    feature_values_long,
    immunomodulator_expr,
    io_target_expr1,
    io_target_expr2,
    io_target_expr3,
    io_target_expr4
  ) %>%
  dplyr::arrange(sample)

cat("Combined all the sample data.", fill = TRUE)

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
gc(TRUE)

# Get only the sample names (no duplicates).
samples <- all_samples %>% dplyr::distinct(sample)

# Build the samples table.
samples <- samples %>%
  dplyr::rename_at("sample", ~("sample_id")) %>%
  merge(til_image_links, by.x = "sample_id", by.y = "sample", all = TRUE) %>%
  dplyr::arrange(sample_id) %>%
  dplyr::rename_at("link", ~("tissue_id")) %>%
  dplyr::select(dplyr::everything()) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(tissue_id = stringi::stri_extract_first(tissue_id, regex = "[\\w]{4}-[\\w]{2}-[\\w]{4}-[\\w]{3}-[\\d]{2}-[\\w]{3}"))
samples %>% .GlobalEnv$write_table_ts(.GlobalEnv$con, "samples", .)
samples <- RPostgres::dbReadTable(.GlobalEnv$con, "samples")

cat("Built the samples table.", fill = TRUE)

# Remove the large til_image_links as we are done with it.
rm(til_image_links)

features <- RPostgres::dbReadTable(.GlobalEnv$con, "features")
groups <- RPostgres::dbReadTable(.GlobalEnv$con, "groups")

samples_to_groups <- dplyr::tibble() %>% tibble::add_column(sample_id = NA, group_id = NA)
features_to_samples <- dplyr::tibble() %>% tibble::add_column(feature_id = NA, sample_id = NA, value = NA)

sample_set_01 <- all_samples %>%
  dplyr::distinct(sample, TCGA_Study, TCGA_Subtype, Immune_Subtype, feature, value) %>%
  dplyr::arrange(sample)

# Remove the HUGE all_samples as we are done with it.
rm(all_samples)

cat("Removed the HUGE all_samples as we are done with it.", fill = TRUE)
gc(TRUE)

cat("Rebuilding samples_to_groups and features_to_samples.", fill = TRUE)

for (row in 1:2) {
# for (row in 1:nrow(samples)) {
  current_id = samples[row, "id"]
  current_sample_id = samples[row, "sample_id"]
  samples_to_groups <- samples_to_groups %>%
    .GlobalEnv$rebuild_samples_to_groups(
      current_id,
      current_sample_id,
      sample_set_01,
      groups
    )

  features_to_samples <- features_to_samples %>%
    .GlobalEnv$rebuild_features_to_samples(
      current_id,
      current_sample_id,
      sample_set_01,
      features
    )
}
rm(row)
rm(current_id)
rm(current_sample_id)

cat("Cleaned up.", fill = TRUE)
gc(TRUE)


samples_to_groups %>% .GlobalEnv$write_table_ts(.GlobalEnv$con, "samples_to_groups", .)

cat("Built samples_to_groups table.", fill = TRUE)

# Remove the data we are done with.
rm(features_to_samples)
rm(features)
rm(groups)
rm(samples)
rm(sample_set_01)
rm(samples_to_groups)

cat("Cleaned up.", fill = TRUE)
gc(TRUE)
