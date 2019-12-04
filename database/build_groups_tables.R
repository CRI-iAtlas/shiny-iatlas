# Get data from feature feather file as a data.frame and convert to a tibble.
groups <- feather::read_feather("data2/groups.feather")
groups <- dplyr::as_tibble(groups) %>%
  dplyr::rename_at("group", ~("name")) %>%
  dplyr::rename_at("group_name", ~("display")) %>%
  tibble::add_column(parent = NA, .after = "color") %>%
  tibble::add_column(subgroup = NA, .after = "color")

parents <- groups %>%
  dplyr::filter(!is.na(parent_group)) %>%
  dplyr::distinct(parent_group, .keep_all = TRUE) %>%
  dplyr::arrange(parent_group) %>%
  dplyr::select(dplyr::starts_with("parent_group")) %>%
  dplyr::rename_at("parent_group", ~("name")) %>%
  dplyr::rename_at("parent_group_display", ~("display")) %>%
  tibble::add_column(characteristics = NA, .after = "display") %>%
  tibble::add_column(parent = NA, .after = "display") %>%
  tibble::add_column(subgroup = NA, .after = "display") %>%
  tibble::add_column(color = NA, .after = "display")


groups_db <- dplyr::bind_rows(groups, parents)
groups_db <- groups_db %>%
  dplyr::arrange(name) %>%
  tibble::add_column(id = 1:nrow(groups_db), .before = "name") %>%
  as.data.frame

groups <- groups_db %>% .GlobalEnv$rebuild_groups(groups_db) %>%
  dplyr::select(-c("parent_group")) %>%
  dplyr::select(-c("parent_group_display")) %>%
  dplyr::select(-c("subtype_group")) %>%
  dplyr::select(-c("subtype_group_display"))
groups %>% .GlobalEnv$write_table_ts(.GlobalEnv$con, "groups", .)

### Clean up ###
# Data
rm(groups)
rm(groups_db)
rm(parents)
