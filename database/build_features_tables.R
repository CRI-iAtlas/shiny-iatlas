# Get data from feature feather file as a data.frame and convert to a tibble.
features <- feather::read_feather("data2/features.feather")
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

# Create the method_tags table with data.
method_tags %>% .GlobalEnv$write_table_ts(.GlobalEnv$con, "method_tags", .)
method_tags <- RPostgres::dbReadTable(.GlobalEnv$con, "method_tags")

# Get data from feature_values feather file as a data.frame and convert to a tibble.
feature_values <- feather::read_feather("data2/feature_values_long.feather")
feature_values <- dplyr::as_tibble(feature_values)

features <- features %>%
  .GlobalEnv$rebuild_features(classes, method_tags) %>%
  dplyr::select(-c("class")) %>%
  dplyr::rename_at("class_int", ~("class")) %>%
  dplyr::select(-c("methods_tag")) %>%
  dplyr::rename_at("mt_int", ~("method_tag_id"))

features %>% .GlobalEnv$write_table_ts(.GlobalEnv$con, "features", .)

### Clean up ###
# Data
# rm(classes)
# rm(feature_values)
# rm(features)
# rm(method_tags)
