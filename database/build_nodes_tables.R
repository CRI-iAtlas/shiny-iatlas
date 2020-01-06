cat(crayon::magenta("Importing feather files for nodes"), fill = TRUE)
nodes <- .GlobalEnv$load_feather_data("../data2/nodes") %>%
  dplyr::distinct(Node, Group, Immune, UpBinRatio) %>%
  dplyr::arrange(Node, Group, Immune, UpBinRatio)
cat(crayon::blue("Imported feather files for nodes"), fill = TRUE)

cat(crayon::magenta("Building the node_names data"), fill = TRUE)
node_names <- nodes %>%
  dplyr::distinct(Node) %>%
  dplyr::rename_at("Node", ~("name"))
cat(crayon::blue("Built the node_names data"), fill = TRUE)

cat(crayon::magenta("Building the node_names table."), fill = TRUE)
table_written <- node_names %>% .GlobalEnv$write_table_ts("node_names")
cat(crayon::blue("Built the node_names table. (", nrow(node_names), "rows )"), fill = TRUE, sep = " ")

cat(crayon::magenta("Building the nodes data"), fill = TRUE)
node_names <- .GlobalEnv$read_table("node_names") %>% dplyr::as_tibble()

nodes <- nodes %>%
  dplyr::rename_at("Node", ~("name")) %>%
  dplyr::inner_join(node_names, by = "name") %>%
  dplyr::rename_at("id", ~("node_name_id")) %>%
  dplyr::rename_at("UpBinRatio", ~("ecn_value")) %>%
  tibble::add_column(id = 1:nrow(nodes), .before = "name")
cat(crayon::blue("Built the nodes data"), fill = TRUE)

cat(crayon::magenta("Building the nodes table."), fill = TRUE)
table_written <- nodes %>%
  dplyr::select(id, ecn_value, node_name_id) %>%
  .GlobalEnv$write_table_ts("nodes")
cat(crayon::blue("Built the nodes table. (", nrow(nodes), "rows )"), fill = TRUE, sep = " ")

cat(crayon::magenta("Building the nodes_to_tags data"), fill = TRUE)
tags <- .GlobalEnv$read_table("tags") %>% dplyr::as_tibble()

node_set_group <- nodes %>%
  dplyr::rename_at("id", ~("node_id")) %>%
  dplyr::inner_join(tags, by = c("Group" = "name")) %>%
  dplyr::rename_at("id", ~("tag_id"))

node_set_immune <- nodes %>%
  dplyr::rename_at("id", ~("node_id")) %>%
  dplyr::inner_join(tags, by = c("Immune" = "name")) %>%
  dplyr::rename_at("id", ~("tag_id"))

nodes_to_tags <- node_set_group %>%
  dplyr::bind_rows(node_set_immune) %>%
  dplyr::distinct(node_id, tag_id)
cat(crayon::blue("Built the nodes_to_tags data"), fill = TRUE)

cat(crayon::magenta("Building the nodes_to_tags table.\n(There are", nrow(nodes_to_tags), "rows to write, this may take a little while.)"), fill = TRUE, sep = " ")
table_written <- nodes_to_tags %>% .GlobalEnv$write_table_ts("nodes_to_tags")
cat(crayon::blue("Built the nodes_to_tags table. (", nrow(nodes_to_tags), "rows )"), fill = TRUE, sep = " ")

cat(crayon::magenta("Importing feather files for edges"), fill = TRUE)
edges <- .GlobalEnv$load_feather_data("../data2/edges") %>%
  dplyr::distinct(From, To, Group, Immune, ratioScore) %>%
  dplyr::rename_at("ratioScore", ~("ratio_score")) %>%
  dplyr::arrange(From, To, Group, Immune)
cat(crayon::blue("Imported feather files for edges"), fill = TRUE)

cat(crayon::magenta("Building the edges data"), fill = TRUE)
edges <- edges %>%
  dplyr::inner_join(nodes, by = c("From" = "name", "Group" = "Group", "Immune" = "Immune")) %>%
  dplyr::rename_at("id", ~("node_1_id")) %>%
  dplyr::inner_join(nodes, by = c("To" = "name", "Group" = "Group", "Immune" = "Immune")) %>%
  dplyr::rename_at("id", ~("node_2_id")) %>%
  dplyr::distinct(node_1_id, node_2_id, ratio_score) %>%
  dplyr::arrange(node_1_id, node_2_id, ratio_score)
cat(crayon::blue("Built the edges data"), fill = TRUE)

cat(crayon::magenta("Building the edges table.\n(There are", nrow(edges), "rows to write, this may take a little while.)"), fill = TRUE, sep = " ")
table_written <- edges %>% .GlobalEnv$write_table_ts("edges")
cat(crayon::blue("Built the edges table. (", nrow(edges), "rows )"), fill = TRUE, sep = " ")

# Clean up.
rm(edges)
rm(nodes)
rm(node_names)
rm(tags)
rm(node_set_group)
rm(node_set_immune)
rm(nodes_to_tags)
rm(table_written)
cat("Cleaned up.", fill = TRUE)
gc()
