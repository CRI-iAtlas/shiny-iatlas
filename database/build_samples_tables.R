# Get data from feather files as data.frames and convert them to a tibbles.
cat("Importing feather files for samples.", fill = TRUE)
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
samples <- RPostgres::dbReadTable(.GlobalEnv$con, "samples") %>% dplyr::as_tibble()
cat("Built the samples table.", fill = TRUE)

# Remove the large til_image_links as we are done with it.
rm(til_image_links)
cat("Cleaned up.", fill = TRUE)
gc()

features <- RPostgres::dbReadTable(.GlobalEnv$con, "features") %>%
  dplyr::as_tibble() %>%
  dplyr::select(id:name)
tags <- RPostgres::dbReadTable(.GlobalEnv$con, "tags") %>%
  dplyr::as_tibble() %>%
  dplyr::select(id:name)

# Reduce the HUGE all_samples set to columns needed so it is smaller.
all_samples <- all_samples %>%
  dplyr::distinct(sample, TCGA_Study, TCGA_Subtype, Immune_Subtype, feature, value) %>%
  dplyr::arrange(sample)

cat("Rebuilding samples_to_tags.", fill = TRUE)
tags_length <- length(tags$name)
tags <- tags %>% as.list
samples_to_tags <- dplyr::tibble() %>% tibble::add_column(sample_id = NA %>% as.integer, tag_id = NA %>% as.integer)

purrr::pmap(tags, ~{
  current_id <- ..1
  current_tag_name <- ..2
  row_number <- which(current_tag_name == tags$name)

  svMisc::progress(row_number, tags_length - 1, progress.bar = TRUE)

  sample_set <- all_samples %>%
    dplyr::distinct(sample, TCGA_Study, TCGA_Subtype, Immune_Subtype) %>%
    dplyr::filter(TCGA_Study == current_tag_name | TCGA_Subtype == current_tag_name | Immune_Subtype == current_tag_name)

  samples_to_tags <<- samples_to_tags %>%
    .GlobalEnv$rebuild_samples_to_tags(current_id, current_tag_name, sample_set, samples)

  if (row_number == tags_length) {
    cat("Rebuilt samples_to_tags.", fill = TRUE)
  }

  rm(current_id)
  rm(current_tag_name)
  rm(row_number)
  rm(sample_set)
})

cat("Cleaned up.", fill = TRUE)
gc()

samples_to_tags %>% .GlobalEnv$write_table_ts(.GlobalEnv$con, "samples_to_tags", .)
cat("Built samples_to_tags table.", fill = TRUE)

cat("Rebuilding samples_to_features.", fill = TRUE)
features_length <- length(features$name)
features <- features %>% as.list
features_to_samples <- dplyr::tibble() %>%
  tibble::add_column(
    feature_id = NA %>% as.integer,
    sample_id = NA %>% as.integer,
    value = NA %>% as.numeric,
    inf_value = NA %>% as.numeric
  )

purrr::pmap(features, ~{
  current_id <- ..1
  current_feature_name <- ..2
  row_number <- which(current_feature_name == features$name)

  svMisc::progress(row_number, features_length - 1, progress.bar = TRUE)

  sample_set <- all_samples %>%
    dplyr::select(sample, feature, value) %>%
    dplyr::filter(feature == current_feature_name) %>%
    dplyr::distinct(sample, feature, .keep_all = TRUE)

  features_to_samples <<- features_to_samples %>%
    .GlobalEnv$rebuild_features_to_samples(current_id, sample_set, samples)

  if (row_number == features_length) {
    cat("Rebuilt samples_to_features.", fill = TRUE)
  }

  rm(current_id)
  rm(current_feature_name)
  rm(row_number)
  rm(sample_set)
})

cat("Cleaned up.", fill = TRUE)
gc()

features_to_samples %>% .GlobalEnv$write_table_ts(.GlobalEnv$con, "features_to_samples", .)
cat("Built features_to_samples table.", fill = TRUE)

# Remove the data we are done with.
rm(features_to_samples)
rm(features)
rm(features_length)
rm(tags)
rm(tags_length)
rm(all_samples)
rm(samples)
rm(samples_to_tags)
cat("Cleaned up.", fill = TRUE)
gc()
