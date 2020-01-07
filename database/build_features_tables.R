cat(crayon::magenta("Importing feather file for features."), fill = TRUE)
features <- feather::read_feather("../data2/features.feather") %>% dplyr::rename_at("feature", ~("name"))
cat(crayon::blue("Imported feather file for features."), fill = TRUE)

cat(crayon::magenta("Building classes data."), fill = TRUE)
classes <- features %>%
  dplyr::filter(!is.na(class)) %>%
  dplyr::distinct(class) %>%
  dplyr::rename_at("class", ~("name")) %>%
  dplyr::arrange(name)
cat(crayon::blue("Built classes data."), fill = TRUE)

# Create the classes table with data.
cat(crayon::magenta("Building classes table."), fill = TRUE)
table_written <- classes %>% .GlobalEnv$write_table_ts("classes")
cat(crayon::blue("Built classes table. (", nrow(classes), "rows )"), fill = TRUE, sep = " ")

cat(crayon::magenta("Building method_tags data."), fill = TRUE)
method_tags <- features %>%
  dplyr::filter(!is.na(methods_tag)) %>%
  dplyr::distinct(methods_tag) %>%
  dplyr::rename_at("methods_tag", ~("name")) %>%
  dplyr::arrange(name)
cat(crayon::blue("Built method_tags data"), fill = TRUE)

# Create the method_tags table with data.
cat(crayon::magenta("Building method_tags table."), fill = TRUE)
table_written <- method_tags %>% .GlobalEnv$write_table_ts("method_tags")
cat(crayon::blue("Built method_tags table. (", nrow(method_tags), "rows )"), fill = TRUE, sep = " ")

cat(crayon::magenta("Building features data."), fill = TRUE)
classes <- .GlobalEnv$read_table("classes") %>% dplyr::as_tibble()
method_tags <- .GlobalEnv$read_table("method_tags") %>% dplyr::as_tibble()
features <- features %>%
  dplyr::left_join(classes, by = c("class" = "name")) %>%
  dplyr::rename_at("id", ~("class_id")) %>%
  dplyr::left_join(method_tags, by = c("methods_tag" = "name")) %>%
  dplyr::rename_at("id", ~("method_tag_id")) %>%
  dplyr::select(name, display, order, unit, class_id, method_tag_id) %>%
  dplyr::arrange(name)
cat(crayon::blue("Built features data"), fill = TRUE)

cat(crayon::magenta("Built features table."), fill = TRUE)
table_written <- features %>% .GlobalEnv$write_table_ts("features")
cat(crayon::blue("Built features table. (", nrow(features), "rows )"), fill = TRUE, sep = " ")

### Clean up ###
# Data
rm(classes)
rm(features)
rm(method_tags)
rm(table_written)

cat("Cleaned up.", fill = TRUE)
gc()
