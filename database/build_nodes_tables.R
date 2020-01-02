cat(crayon::magenta("Importing feather files for nodes"), fill = TRUE)
nodes <- .GlobalEnv$load_feather_data("../data3/nodes") %>%
  dplyr::distinct(Node, Group, Immune, UpBinRatio) %>%
  dplyr::arrange(Node)
cat(crayon::blue("Imported feather files for nodes"), fill = TRUE)

cat(crayon::magenta("Building the node_names table."), fill = TRUE)
table_written <- nodes %>%
  dplyr::distinct(Node) %>%
  dplyr::rename_at("Node", ~("name")) %>%
  .GlobalEnv$write_table_ts("node_names")
cat(crayon::blue("Built the node_names table."), fill = TRUE)

cat(crayon::magenta("Building the nodes table."), fill = TRUE)
node_names <- .GlobalEnv$read_table("node_names") %>% dplyr::as_tibble()
table_written <- nodes %>%
  dplyr::select(Node, UpBinRatio) %>%
  dplyr::rename_at("UpBinRatio", ~("ecn_value")) %>%
  dplyr::inner_join(node_names, by = c("Node" = "name")) %>%
  dplyr::rename_at("id", ~("node_name_id")) %>%
  dplyr::select(-c(Node)) %>%
  .GlobalEnv$write_table_ts("nodes")
cat(crayon::blue("Built the nodes table."), fill = TRUE)

cat(crayon::magenta("Building the nodes_to_tags data"), fill = TRUE)
tags <- .GlobalEnv$read_table("tags") %>% dplyr::as_tibble()

current_pool <- .GlobalEnv$pool %>% pool::poolCheckout()
nodes_db <- current_pool %>% 
  dplyr::tbl("nodes") %>%
  dplyr::inner_join(current_pool %>% dplyr::tbl("node_names"), by = c("node_name_id" = "id")) %>%
  dplyr::as_tibble()
current_pool %>% pool::poolReturn()

node_set_group <- nodes %>%
  dplyr::inner_join(tags, by = c("Group" = "name")) %>%
  dplyr::rename_at("id", ~("tag_id")) %>%
  dplyr::inner_join(nodes_db, by = c("Node" = "name")) %>%
  dplyr::rename_at("id", ~("node_id")) %>%
  dplyr::distinct(node_id, tag_id)

node_set_immune <- nodes %>%
  dplyr::inner_join(tags, by = c("Immune" = "name")) %>%
  dplyr::rename_at("id", ~("tag_id")) %>%
  dplyr::inner_join(nodes_db, by = c("Node" = "name")) %>%
  dplyr::rename_at("id", ~("node_id")) %>%
  dplyr::distinct(node_id, tag_id)
cat(crayon::blue("Built the nodes_to_tags data"), fill = TRUE)

cat(crayon::magenta("Building the nodes_to_tags table."), fill = TRUE)
table_written <- node_set_group %>%
  dplyr::bind_rows(node_set_immune) %>%
  dplyr::distinct(node_id, tag_id) %>%
  .GlobalEnv$write_table_ts("nodes_to_tags")
cat(crayon::blue("Built the nodes_to_tags table."), fill = TRUE)
