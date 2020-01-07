cat(crayon::magenta("Importing feather file for tags."), fill = TRUE)
initial_tags <- feather::read_feather("../data2/groups.feather") %>%
  dplyr::as_tibble() %>%
  dplyr::rename_at("group", ~("name")) %>%
  dplyr::rename_at("group_name", ~("display"))
cat(crayon::blue("Imported feather file for tags."), fill = TRUE)

cat(crayon::magenta("Building tags data"), fill = TRUE)
parents <- initial_tags %>%
  dplyr::filter(!is.na(parent_group)) %>%
  dplyr::distinct(parent_group, .keep_all = TRUE) %>%
  dplyr::select(parent_group, parent_group_display) %>%
  dplyr::rename_at("parent_group", ~("name")) %>%
  dplyr::rename_at("parent_group_display", ~("display")) %>%
  tibble::add_column(characteristics = NA, color = NA, .after = "display") %>%
  dplyr::arrange(name)
subtype <- initial_tags %>%
  dplyr::filter(!is.na(subtype_group)) %>%
  dplyr::distinct(subtype_group, .keep_all = TRUE) %>%
  dplyr::select(subtype_group, subtype_group_display) %>%
  dplyr::rename_at("subtype_group", ~("name")) %>%
  dplyr::rename_at("subtype_group_display", ~("display")) %>%
  tibble::add_column(characteristics = NA, color = NA, .after = "display") %>%
  dplyr::arrange(name)
tags <- parents %>%
  dplyr::bind_rows(initial_tags, subtype) %>%
  dplyr::arrange(name) %>%
  dplyr::distinct(name, .keep_all = TRUE)
cat(crayon::blue("Built tags data"), fill = TRUE)

cat(crayon::magenta("Building tags table."), fill = TRUE)
table_written <- tags %>%
  dplyr::select(-c("parent_group", "parent_group_display", "subtype_group", "subtype_group_display")) %>%
  .GlobalEnv$write_table_ts("tags")
cat(crayon::blue("Built tags table. (", nrow(tags), "rows )"), fill = TRUE, sep = " ")

cat(crayon::magenta("Building tags_to_tags data."), fill = TRUE)
tags_db <- .GlobalEnv$read_table("tags") %>%
  dplyr::as_tibble() %>%
  dplyr::select(id, name)
all_tags_with_tag_ids <- tags %>%
  dplyr::inner_join(tags_db, by = "name") %>%
  dplyr::select(id, parent_group, subtype_group) %>%
  dplyr::rename_at("id", ~("tag_id"))
related_parent_tags <- all_tags_with_tag_ids %>%
  dplyr::rename_at("parent_group", ~("name")) %>%
  dplyr::inner_join(tags_db, by = "name") %>%
  dplyr::select(tag_id, id) %>%
  dplyr::rename_at("id", ~("related_tag_id"))
related_subtype_tags <- all_tags_with_tag_ids %>%
  dplyr::rename_at("subtype_group", ~("name")) %>%
  dplyr::inner_join(tags_db, by = "name") %>%
  dplyr::select(tag_id, id) %>%
  dplyr::rename_at("id", ~("related_tag_id"))
tags_to_tags <- related_parent_tags %>%
  dplyr::bind_rows(related_subtype_tags) %>%
  dplyr::distinct(tag_id, related_tag_id)
cat(crayon::magenta("Built tags_to_tags data."), fill = TRUE)

cat(crayon::magenta("Building tags_to_tags table."), fill = TRUE)
table_written <- tags_to_tags %>% .GlobalEnv$write_table_ts("tags_to_tags")
cat(crayon::magenta("Built tags_to_tags table. (", nrow(tags_to_tags), "rows )"), fill = TRUE, sep = " ")

### Clean up ###
# Data
rm(all_tags_with_tag_ids)
rm(initial_tags)
rm(parents)
rm(related_parent_tags)
rm(related_subtype_tags)
rm(subtype)
rm(tags)
rm(tags_db)
rm(tags_to_tags)
rm(table_written)

cat("Cleaned up.", fill = TRUE)
gc()
