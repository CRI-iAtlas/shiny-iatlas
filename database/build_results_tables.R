cat(crayon::magenta("Importing feather files for results."), fill = TRUE)
driver_results1 <- feather::read_feather("../data2/driver_results1.feather")
driver_results2 <- feather::read_feather("../data2/driver_results2.feather")
cat(crayon::blue("Imported feather files for results."), fill = TRUE)

cat(crayon::magenta("Building results data."), fill = TRUE)
all_results <- dplyr::bind_rows(driver_results1, driver_results2)
cat(crayon::blue("Built results data."), fill = TRUE)

# Clean up.
rm(driver_results1)
rm(driver_results2)
cat("Cleaned up.", fill = TRUE)
gc()

cat(crayon::magenta("Building result_labels data."), fill = TRUE)
result_labels <- all_results %>%
  dplyr::rename_at("label", ~("name")) %>%
  dplyr::distinct(name) %>%
  dplyr::arrange(name)
cat(crayon::blue("Built result_labels data."), fill = TRUE)

cat(crayon::magenta("Building result_labels table."), fill = TRUE)
table_written <- result_labels %>% .GlobalEnv$write_table_ts(.GlobalEnv$con, "result_labels", .)
result_labels <- RPostgres::dbReadTable(.GlobalEnv$con, "result_labels") %>% dplyr::as_tibble()
cat(crayon::blue("Built result_labels table."), fill = TRUE)

cat(crayon::magenta("Building results data."), fill = TRUE)
tags <- RPostgres::dbReadTable(.GlobalEnv$con, "tags") %>%
  dplyr::as_tibble() %>%
  dplyr::select(id, name)
results <- all_results %>%
  dplyr::inner_join(result_labels, by = c("label" = "name")) %>%
  dplyr::rename_at("id", ~("label_id")) %>%
  dplyr::inner_join(tags, by = c("group" = "name")) %>%
  dplyr::rename_at("id", ~("tag_id")) %>%
  dplyr::select(-c("group", "label", "parent_group")) %>%
  dplyr::rename_at("pvalue", ~("p_value")) %>%
  dplyr::rename_at("log10_pvalue", ~("log10_p_value")) %>%
  tibble::add_column(id = 1:nrow(all_results), .before = "n_wt")
cat(crayon::blue("Built results data."), fill = TRUE)

cat(crayon::magenta("Building results table.\n(Please be patient, this may take a little while as there are many rows to write.)"), fill = TRUE)
table_written <- results %>% dplyr::select(-c("feature")) %>% .GlobalEnv$write_table_ts(.GlobalEnv$con, "results", .)
cat(crayon::blue("Built results table."), fill = TRUE)

cat(crayon::magenta("Building features_to_results data."), fill = TRUE)
features <- RPostgres::dbReadTable(.GlobalEnv$con, "features") %>%
  dplyr::as_tibble() %>%
  dplyr::select(id, name) %>%
  dplyr::rename_at("id", ~("feature_id"))
features_to_results <- results %>%
  dplyr::select(id, feature) %>%
  dplyr::rename_at("id", ~("result_id")) %>%
  dplyr::inner_join(features, by = c("feature" = "name")) %>%
  dplyr::distinct(feature_id, result_id)
cat(crayon::blue("Built features_to_results data."), fill = TRUE)

cat(crayon::magenta("Building features_to_results table.\n(Please be patient, this may take a little while as there are many rows to write.)"), fill = TRUE)
table_written <- features_to_results %>% .GlobalEnv$write_table_ts(.GlobalEnv$con, "features_to_results", .)
cat(crayon::blue("Built features_to_results table."), fill = TRUE)

# Remove the data we are done with.
rm(all_results)
rm(features)
rm(features_to_results)
rm(result_labels)
rm(results)
rm(table_written)
rm(tags)
cat("Cleaned up.", fill = TRUE)
gc()
