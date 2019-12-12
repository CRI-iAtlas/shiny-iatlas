# Get data from groups feather file as a data.frame and convert to a tibble.
tags <- feather::read_feather("../data2/groups.feather")

cat("Imported feather file for tags.", fill = TRUE)

tags <- dplyr::as_tibble(tags) %>%
  dplyr::rename_at("group", ~("name")) %>%
  dplyr::rename_at("group_name", ~("display")) %>%
  tibble::add_column(parent = NA, subgroup = NA, .after = "color")

parents <- tags %>%
  dplyr::filter(!is.na(parent_group)) %>%
  dplyr::distinct(parent_group, .keep_all = TRUE) %>%
  dplyr::arrange(parent_group) %>%
  dplyr::select(dplyr::starts_with("parent_group")) %>%
  dplyr::rename_at("parent_group", ~("name")) %>%
  dplyr::rename_at("parent_group_display", ~("display")) %>%
  tibble::add_column(characteristics = NA, parent = NA, subgroup = NA, color = NA, .after = "display")

tags_db <- dplyr::bind_rows(tags, parents)
tags_db <- tags_db %>%
  dplyr::arrange(name) %>%
  tibble::add_column(id = 1:nrow(tags_db), .before = "name")

tags <- tags_db %>% .GlobalEnv$rebuild_tags(tags_db) %>%
  dplyr::select(-c("parent_group")) %>%
  dplyr::select(-c("parent_group_display")) %>%
  dplyr::select(-c("subtype_group")) %>%
  dplyr::select(-c("subtype_group_display"))
tags %>% .GlobalEnv$write_table_ts(.GlobalEnv$con, "tags", .)

cat("Built tags table.", fill = TRUE)

### Clean up ###
# Data
rm(tags)
rm(tags_db)
rm(parents)

cat("Cleaned up.", fill = TRUE)
gc()
