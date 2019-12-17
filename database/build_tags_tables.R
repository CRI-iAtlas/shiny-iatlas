cat(crayon::magenta("Importing feather file for tags."), fill = TRUE)
tags <- feather::read_feather("../data2/groups.feather") %>%
  dplyr::as_tibble() %>%
  dplyr::rename_at("group", ~("name")) %>%
  dplyr::rename_at("group_name", ~("display")) %>%
  tibble::add_column(parent = NA, subgroup = NA, .after = "color")
cat(crayon::blue("Imported feather file for tags."), fill = TRUE)

cat(crayon::magenta("Building tags data"), fill = TRUE)

parents <- tags %>%
  dplyr::filter(!is.na(parent_group)) %>%
  dplyr::distinct(parent_group, .keep_all = TRUE) %>%
  dplyr::select(parent_group, parent_group_display) %>%
  dplyr::rename_at("parent_group", ~("name")) %>%
  dplyr::rename_at("parent_group_display", ~("display")) %>%
  tibble::add_column(characteristics = NA, parent = NA, subgroup = NA, color = NA, .after = "display") %>%
  dplyr::arrange(name)

tags_db <- tags %>% dplyr::bind_rows(parents) %>% dplyr::arrange(name)

tags_db <- tags_db %>% tibble::add_column(id = 1:nrow(tags_db), .before = "name")

cat(crayon::magenta("Building tags table."), fill = TRUE)
table_written <-  tags_db %>%
  .GlobalEnv$rebuild_tags(tags_db) %>%
  dplyr::select(-c("parent_group", "parent_group_display", "subtype_group", "subtype_group_display")) %>%
  .GlobalEnv$write_table_ts("tags")
cat(crayon::blue("Built tags table."), fill = TRUE)

### Clean up ###
# Data
rm(tags)
rm(tags_db)
rm(parents)
rm(table_written)

cat("Cleaned up.", fill = TRUE)
gc()
