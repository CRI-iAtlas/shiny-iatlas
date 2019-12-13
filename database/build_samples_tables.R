# Get data from feather files as data.frames and convert them to a tibbles.
driver_mutations1 <-
  feather::read_feather("../data2/driver_mutations1.feather")
driver_mutations2 <-
  feather::read_feather("../data2/driver_mutations2.feather")
driver_mutations3 <-
  feather::read_feather("../data2/driver_mutations3.feather")
driver_mutations4 <-
  feather::read_feather("../data2/driver_mutations4.feather")
driver_mutations5 <-
  feather::read_feather("../data2/driver_mutations5.feather")
feature_values_long <-
  feather::read_feather("../data2/feature_values_long.feather")
immunomodulator_expr <-
  feather::read_feather("../data2/immunomodulator_expr.feather")
io_target_expr1 <-
  feather::read_feather("../data2/io_target_expr1.feather")
io_target_expr2 <-
  feather::read_feather("../data2/io_target_expr2.feather")
io_target_expr3 <-
  feather::read_feather("../data2/io_target_expr3.feather")
io_target_expr4 <-
  feather::read_feather("../data2/io_target_expr4.feather")
til_image_links <-
  feather::read_feather("../data2/til_image_links.feather")

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
gc()

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
tags <- RPostgres::dbReadTable(.GlobalEnv$con, "tags") %>%
  dplyr::as_tibble() %>%
  dplyr::select(id:name)

samples_to_tags <- dplyr::tibble() %>% tibble::add_column(sample_id = NA, tag_id = NA)
features_to_samples <- dplyr::tibble() %>% tibble::add_column(feature_id = NA, sample_id = NA, value = NA)

# Reduce the HUGE all_samples set to columns needed so it is smaller.
all_samples <- all_samples %>%
  dplyr::distinct(sample, TCGA_Study, TCGA_Subtype, Immune_Subtype, feature, value) %>%
  dplyr::arrange(sample)

cat("Rebuilding samples_to_tags and features_to_samples.", fill = TRUE)
samples_length <- length(samples$sample_id)

samples <- samples %>% dplyr::select(id:sample_id) %>% as.list

purrr::pmap(samples, ~{
  current_id <- ..1
  current_sample_id <- ..2
  row_number <- which(current_sample_id == samples$sample_id)

  svMisc::progress(row_number, samples_length - 1, progress.bar = TRUE)

  sample_set <- all_samples %>%
    dplyr::filter(sample == current_sample_id)
  
  sample_set_tags <- sample_set %>%
    dplyr::distinct(sample, TCGA_Study, TCGA_Subtype, Immune_Subtype)

  samples_to_tags <<- samples_to_tags %>%
    .GlobalEnv$rebuild_samples_to_tags(
      current_id,
      current_sample_id,
      sample_set_tags,
      tags
    )
  
  # sample_set_features <- sample_set %>%
  #   dplyr::distinct(sample, feature, value)
  # 
  # features_to_samples <- features_to_samples %>%
  #   .GlobalEnv$rebuild_features_to_samples(
  #     current_id,
  #     current_sample_id,
  #     sample_set_features,
  #     features
  #   )

  if (row_number == samples_length) {
    cat("Rebuilt samples_to_tags and features_to_samples.", fill = TRUE)
  }

  rm(current_id)
  rm(current_sample_id)
  rm(row_number)
  rm(sample_set)
})

# for (row in 1:nrow(samples)) {
#   svMisc::progress(row, nrow(samples) - 1, progress.bar = TRUE)
#   current_id = samples[row, "id"]
#   current_sample_id <- samples[row, "sample_id"]
#   # samples_to_tags <- samples_to_tags %>%
#   #   .GlobalEnv$rebuild_samples_to_tags(
#   #     current_id,
#   #     current_sample_id,
#   #     sample_set_01,
#   #     tags
#   #   )
#   # 
  # features_to_samples <- features_to_samples %>%
  #   .GlobalEnv$rebuild_features_to_samples(
  #     current_id,
  #     current_sample_id,
  #     sample_set_01,
  #     features
  #   )
#   if (row == nrow(samples)) {
#     cat("\nRebuilt samples_to_tags and features_to_samples.", fill = TRUE)
#   }
#   rm(row)
#   rm(current_id)
#   rm(current_sample_id)
# }

cat("Cleaned up.", fill = TRUE)
gc()

samples_to_tags %>% .GlobalEnv$write_table_ts(.GlobalEnv$con, "samples_to_tags", .)

cat("Built samples_to_tags table.", fill = TRUE)

# features_to_samples %>% .GlobalEnv$write_table_ts(.GlobalEnv$con, "features_to_samples", .)

cat("Built features_to_samples table.", fill = TRUE)

# Remove the data we are done with.
rm(features_to_samples)
rm(features)
rm(tags)
rm(samples)
rm(samples_to_tags)

cat("Cleaned up.", fill = TRUE)
gc()
