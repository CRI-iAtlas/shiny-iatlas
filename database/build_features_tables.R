# Get data from feature feather file as a data.frame and convert to a tibble.
cat("Importing feather file for features.", fill = TRUE)
features <- feather::read_feather("../data2/features.feather") %>% dplyr::rename_at("feature", ~("name"))
cat("Imported feather file for features.", fill = TRUE)

classes <- features %>%
  dplyr::filter(!is.na(class)) %>%
  dplyr::distinct(class) %>%
  dplyr::arrange(class) %>%
  dplyr::select(class) %>%
  dplyr::transmute(name = class)

# Create the classes table with data.
classes %>% .GlobalEnv$write_table_ts(.GlobalEnv$con, "classes", .)
classes <- RPostgres::dbReadTable(.GlobalEnv$con, "classes") %>%
  dplyr::as_tibble() %>%
  dplyr::select(id:name)
cat("Built classes table.", fill = TRUE)

method_tags <- features %>%
  dplyr::filter(!is.na(methods_tag)) %>%
  dplyr::distinct(methods_tag) %>%
  dplyr::arrange(methods_tag) %>%
  dplyr::select(methods_tag) %>%
  dplyr::transmute(name = methods_tag)

# Create the method_tags table with data.
method_tags %>% .GlobalEnv$write_table_ts(.GlobalEnv$con, "method_tags", .)
method_tags <- RPostgres::dbReadTable(.GlobalEnv$con, "method_tags") %>%
  dplyr::as_tibble() %>%
  dplyr::select(id:name)
cat("Built method_tags table.", fill = TRUE)

features <- features %>%
  dplyr::left_join(classes, by = c("class" = "name")) %>%
  dplyr::rename_at("id", ~("class_id")) %>%
  dplyr::left_join(method_tags, by = c("methods_tag" = "name")) %>%
  dplyr::rename_at("id", ~("method_tag_id")) %>%
  dplyr::select(name, display, order, unit, class_id, method_tag_id) %>%
  dplyr::arrange(name)

features %>% .GlobalEnv$write_table_ts(.GlobalEnv$con, "features", .)
cat("Built features table.", fill = TRUE)

### Clean up ###
# Data
rm(classes)
rm(features)
rm(method_tags)

cat("Cleaned up.", fill = TRUE)
gc()
