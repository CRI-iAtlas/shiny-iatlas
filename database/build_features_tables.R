# Get data from feature feather file as a data.frame and convert to a tibble.
features <- feather::read_feather("data2/features.feather")

cat("Imported feather file for features.", fill = TRUE)

features <- dplyr::as_tibble(features) %>%
  dplyr::rename_at("feature", ~("name")) %>%
  tibble::add_column(class_int = NA, .after = "class") %>%
  tibble::add_column(mt_int = NA, .before = "methods_tag")

classes <- features %>%
  dplyr::filter(!is.na(class)) %>%
  dplyr::distinct(class) %>%
  dplyr::arrange(class) %>%
  dplyr::select(class) %>%
  dplyr::transmute(name = class)

method_tags <- features %>%
  dplyr::filter(!is.na(methods_tag)) %>%
  dplyr::distinct(methods_tag) %>%
  dplyr::arrange(methods_tag) %>%
  dplyr::select(methods_tag) %>%
  dplyr::transmute(name = methods_tag)

# Create the classes table with data.
classes %>% .GlobalEnv$write_table_ts(.GlobalEnv$con, "classes", .)
classes <- RPostgres::dbReadTable(.GlobalEnv$con, "classes")

cat("Built classes table.", fill = TRUE)

# Create the method_tags table with data.
method_tags %>% .GlobalEnv$write_table_ts(.GlobalEnv$con, "method_tags", .)
method_tags <- RPostgres::dbReadTable(.GlobalEnv$con, "method_tags")

cat("Built method_tags table.", fill = TRUE)

features <- features %>%
  .GlobalEnv$rebuild_features(classes, method_tags) %>%
  dplyr::select(-c("class")) %>%
  dplyr::rename_at("class_int", ~("class")) %>%
  dplyr::select(-c("methods_tag")) %>%
  dplyr::rename_at("mt_int", ~("method_tag_id"))

features %>% .GlobalEnv$write_table_ts(.GlobalEnv$con, "features", .)

cat("Built features table.", fill = TRUE)

### Clean up ###
# Data
rm(classes)
rm(features)
rm(method_tags)

cat("Cleaned up.", fill = TRUE)
gc(TRUE)
