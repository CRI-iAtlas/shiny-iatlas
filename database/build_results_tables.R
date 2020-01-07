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
table_written <- result_labels %>% .GlobalEnv$write_table_ts("result_labels")
cat(crayon::blue("Built result_labels table. (", nrow(result_labels), "rows )"), fill = TRUE, sep = " ")

cat(crayon::magenta("Building results data."), fill = TRUE)
result_labels <- .GlobalEnv$read_table("result_labels") %>% dplyr::as_tibble()
tags <- .GlobalEnv$read_table("tags") %>%
  dplyr::as_tibble() %>%
  dplyr::select(id, name) %>%
  dplyr::rename_at("id", ~("tag_id"))
features <- .GlobalEnv$read_table("features") %>%
  dplyr::as_tibble() %>%
  dplyr::select(id, name) %>%
  dplyr::rename_at("id", ~("feature_id"))
results <- all_results %>%
  dplyr::inner_join(result_labels, by = c("label" = "name")) %>%
  dplyr::rename_at("id", ~("label_id")) %>%
  dplyr::rename_at("pvalue", ~("p_value")) %>%
  dplyr::rename_at("log10_pvalue", ~("log10_p_value")) %>%
  dplyr::inner_join(tags, by = c("group" = "name")) %>%
  dplyr::inner_join(features, by = c("feature" = "name")) %>%
  dplyr::select(-c("feature", "group", "label", "parent_group"))
cat(crayon::blue("Built results data."), fill = TRUE)

cat(crayon::magenta("Building results table.\n(Please be patient, this may take a little while as there are", nrow(results), "rows to write.)"), fill = TRUE, spe = " ")
table_written <- results %>% .GlobalEnv$write_table_ts("results")
cat(crayon::blue("Built results table. (", nrow(results), "rows )"), fill = TRUE, sep = " ")

# Remove the data we are done with.
rm(all_results)
rm(features)
rm(result_labels)
rm(results)
rm(table_written)
rm(tags)
cat("Cleaned up.", fill = TRUE)
gc()
