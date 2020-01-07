cat(crayon::magenta("Importing driver mutation feather files for genes"), fill = TRUE)
driver_mutations <- dplyr::bind_rows(
  feather::read_feather("../data2/driver_mutations1.feather"),
  feather::read_feather("../data2/driver_mutations2.feather"),
  feather::read_feather("../data2/driver_mutations3.feather"),
  feather::read_feather("../data2/driver_mutations4.feather"),
  feather::read_feather("../data2/driver_mutations5.feather")
)
cat(crayon::blue("Imported driver mutation feather files for genes"), fill = TRUE)

cat(crayon::magenta("Importing immunomodulators and io_target feather files for genes"), fill = TRUE)
immunomodulator_expr <- feather::read_feather("../data2/immunomodulator_expr.feather")
immunomodulators <- feather::read_feather("../data2/immunomodulators.feather") %>%
  dplyr::filter(!is.na(gene)) %>%
  dplyr::mutate_at(dplyr::vars(entrez), as.numeric) %>%
  dplyr::rename_at("display", ~("canonical")) %>%
  dplyr::rename_at("display2", ~("display")) %>%
  tibble::add_column(
    pathway = NA %>% as.character,
    therapy_type = NA %>% as.character,
    description = NA %>% as.character,
    references = NA,
    type = "immunomodulator" %>% as.character
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(references = paste("{", reference %>% base::strsplit("\\s\\|\\s") %>% stringi::stri_join_list(sep = ','), "}", sep = ""))
io_target_expr <- dplyr::bind_rows(
  feather::read_feather("../data2/io_target_expr1.feather"),
  feather::read_feather("../data2/io_target_expr2.feather"),
  feather::read_feather("../data2/io_target_expr3.feather"),
  feather::read_feather("../data2/io_target_expr4.feather")
)
io_targets <-
  feather::read_feather("../data2/io_targets.feather") %>%
  dplyr::filter(!is.na(gene)) %>%
  dplyr::distinct(gene, .keep_all = TRUE) %>%
  dplyr::mutate_at(dplyr::vars(entrez), as.numeric) %>%
  dplyr::rename_at("display", ~("canonical")) %>%
  dplyr::rename_at("display2", ~("display")) %>%
  tibble::add_column(
    gene_family = NA %>% as.character,
    super_category = NA %>% as.character,
    immune_checkpoint = NA %>% as.character,
    gene_function = NA %>% as.character,
    references = NA,
    type = "io_target" %>% as.character
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(references = .GlobalEnv$link_to_references(link))
cat(crayon::blue("Imported immunomodulators and io_target feather files for genes"), fill = TRUE)

cat(crayon::magenta("Binding gene expr data."), fill = TRUE)
all_genes_expr <- driver_mutations %>%
  dplyr::bind_rows(immunomodulator_expr, io_target_expr) %>%
  dplyr::distinct(gene)
cat(crayon::blue("Bound gene expr data."), fill = TRUE)

cat(crayon::magenta("Binding immunomodulators and io_targets."), fill = TRUE)
io_targets <- io_targets %>%
  dplyr::select(dplyr::everything()) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(gene_family = .GlobalEnv$switch_value(.data, "gene", "gene_family", immunomodulators),
    super_category = .GlobalEnv$switch_value(.data, "gene", "super_category", immunomodulators),
    immune_checkpoint = .GlobalEnv$switch_value(.data, "gene", "immune_checkpoint", immunomodulators),
    gene_function = .GlobalEnv$switch_value(.data, "gene", "gene_function", immunomodulators),
    references = .GlobalEnv$switch_value(.data, "gene", "references", immunomodulators)
  )
immunomodulators <- immunomodulators %>%
  dplyr::select(-c("reference")) %>%
  dplyr::anti_join(io_targets, by = "gene")
immunomodulators_and_io_targets <- immunomodulators %>% dplyr::bind_rows(io_targets)
cat(crayon::blue("Bound immunomodulators and io_targets."), fill = TRUE)

cat(crayon::magenta("Building all gene data."), fill = TRUE)
joined_genes <- all_genes_expr %>% dplyr::anti_join(immunomodulators_and_io_targets, by = "gene")
all_genes <- immunomodulators_and_io_targets %>%
  dplyr::bind_rows(joined_genes) %>%
  dplyr::as_tibble() %>%
  dplyr::rename_at("gene", ~("hgnc")) %>%
  dplyr::arrange(hgnc)
cat(crayon::blue("Built all gene data."), fill = TRUE)

# Clean up.
rm(all_genes_expr)
rm(io_targets)
rm(joined_genes)
rm(immunomodulators)
rm(immunomodulators_and_io_targets)
cat("Cleaned up.", fill = TRUE)
gc()

cat(crayon::magenta("Building gene_types data."), fill = TRUE)
gene_types <- dplyr::tibble(
  name = c("immunomodulator", "io_target", "driver_mutation", "extra_cellular_network"),
  display = c("Immunomodulator", "IO Target", "Driver Mutation", "Extra Cellular Network")
)
cat(crayon::blue("Built gene_types data."), fill = TRUE)

cat(crayon::magenta("Building gene_types table."), fill = TRUE)
table_written <- gene_types %>% .GlobalEnv$write_table_ts("gene_types")
cat(crayon::blue("Built gene_types table. (", nrow(gene_types), "rows )"), fill = TRUE, sep = " ")

cat(crayon::magenta("Building immune_checkpoints data."), fill = TRUE)
immune_checkpoints <- all_genes %>% .GlobalEnv$rebuild_gene_relational_data("immune_checkpoint", "name")
cat(crayon::blue("Built immune_checkpoints data."), fill = TRUE)

cat(crayon::magenta("Building immune_checkpoints table."), fill = TRUE)
table_written <- immune_checkpoints %>% .GlobalEnv$write_table_ts("immune_checkpoints")
cat(crayon::blue("Built immune_checkpoints table. (", nrow(immune_checkpoints), "rows )"), fill = TRUE, sep = " ")

cat(crayon::magenta("Building gene_families data."), fill = TRUE)
gene_families <- all_genes %>% .GlobalEnv$rebuild_gene_relational_data("gene_family", "name")
cat(crayon::blue("Built gene_families data."), fill = TRUE)

cat(crayon::magenta("Building gene_families table."), fill = TRUE)
table_written <- gene_families %>% .GlobalEnv$write_table_ts("gene_families")
cat(crayon::blue("Built gene_families table. (", nrow(gene_families), "rows )"), fill = TRUE, sep = " ")

cat(crayon::magenta("Building gene_functions data."), fill = TRUE)
gene_functions <- all_genes %>% .GlobalEnv$rebuild_gene_relational_data("gene_function", "name")
cat(crayon::blue("Built gene_functions data."), fill = TRUE)

cat(crayon::magenta("Building gene_functions table."), fill = TRUE)
table_written <- gene_functions %>% .GlobalEnv$write_table_ts("gene_functions")
cat(crayon::blue("Built gene_functions table. (", nrow(gene_functions), "rows )"), fill = TRUE, sep = " ")

cat(crayon::magenta("Built pathways data."), fill = TRUE)
pathways <- all_genes %>% .GlobalEnv$rebuild_gene_relational_data("pathway", "name")
cat(crayon::blue("Built pathways data."), fill = TRUE)

cat(crayon::magenta("Built pathways table."), fill = TRUE)
table_written <- pathways %>% .GlobalEnv$write_table_ts("pathways")
cat(crayon::blue("Built pathways table. (", nrow(pathways), "rows )"), fill = TRUE, sep = " ")

cat(crayon::magenta("Building super_categories data."), fill = TRUE)
super_categories <- all_genes %>% .GlobalEnv$rebuild_gene_relational_data("super_category", "name")
cat(crayon::blue("Built super_categories data."), fill = TRUE)

cat(crayon::magenta("Building super_categories table."), fill = TRUE)
table_written <- super_categories %>% .GlobalEnv$write_table_ts("super_categories")
cat(crayon::blue("Built super_categories table. (", nrow(super_categories), "rows )"), fill = TRUE, sep = " ")

cat(crayon::magenta("Built therapy_types data."), fill = TRUE)
therapy_types <- all_genes %>% .GlobalEnv$rebuild_gene_relational_data("therapy_type", "name")
cat(crayon::blue("Built therapy_types data."), fill = TRUE)

cat(crayon::magenta("Built therapy_types table."), fill = TRUE)
table_written <- therapy_types %>% .GlobalEnv$write_table_ts("therapy_types")
cat(crayon::blue("Built therapy_types table. (", nrow(therapy_types), "rows )"), fill = TRUE, sep = " ")

cat(crayon::magenta("Building genes data."), fill = TRUE)
immune_checkpoints <- .GlobalEnv$read_table("immune_checkpoints")
gene_families <- .GlobalEnv$read_table("gene_families")
gene_functions <- .GlobalEnv$read_table("gene_functions")
pathways <- .GlobalEnv$read_table("pathways")
super_categories <- .GlobalEnv$read_table("super_categories")
therapy_types <- .GlobalEnv$read_table("therapy_types")
cat(crayon::cyan("Adding immune_checkpoint ids."), fill = TRUE)
genes <- all_genes %>%
  dplyr::full_join(immune_checkpoints, by = c("immune_checkpoint" = "name")) %>%
  dplyr::rename_at("id", ~("immune_checkpoint_id"))
cat(crayon::cyan("Adding gene_family ids."), fill = TRUE)
genes <- genes %>%
  dplyr::full_join(gene_families, by = c("gene_family" = "name")) %>%
  dplyr::rename_at("id", ~("gene_family_id"))
cat(crayon::cyan("Adding gene_function ids."), fill = TRUE)
genes <- genes %>%
  dplyr::full_join(gene_functions, by = c("gene_function" = "name")) %>%
  dplyr::rename_at("id", ~("gene_function_id"))
cat(crayon::cyan("Adding pathway ids."), fill = TRUE)
genes <- genes %>%
  dplyr::full_join(pathways, by = c("pathway" = "name")) %>%
  dplyr::rename_at("id", ~("pathway_id"))
cat(crayon::cyan("Adding super_category ids."), fill = TRUE)
genes <- genes %>%
  dplyr::full_join(super_categories, by = c("super_category" = "name")) %>%
  dplyr::rename_at("id", ~("super_cat_id"))
cat(crayon::cyan("Adding therapy_type ids."), fill = TRUE)
genes <- genes %>%
  dplyr::full_join(therapy_types, by = c("therapy_type" = "name")) %>%
  dplyr::rename_at("id", ~("therapy_type_id")) %>%
  dplyr::select(-c("gene_family", "gene_function", "immune_checkpoint", "link", "pathway", "super_category", "therapy_type", "type"))
cat(crayon::blue("Built genes data."), fill = TRUE)

cat(crayon::magenta("Building genes table."), fill = TRUE)
table_written <- genes %>% .GlobalEnv$write_table_ts("genes")
cat(crayon::blue("Built genes table. (", nrow(genes), "rows )"), fill = TRUE, sep = " ")

# Clean up.
rm(all_genes)
rm(super_categories)
rm(gene_families)
rm(immune_checkpoints)
rm(gene_functions)
rm(pathways)
rm(therapy_types)
cat("Cleaned up.", fill = TRUE)
gc()

cat(crayon::magenta("Building genes_to_types data."), fill = TRUE)
genes <- .GlobalEnv$read_table("genes") %>% dplyr::select(id, hgnc)
gene_types <- .GlobalEnv$read_table("gene_types")

# Collect the ids of the 3 gene_types.
driver_mutation_id <- gene_types %>% dplyr::filter(name == "driver_mutation") %>% .[["id"]]
immunomodulator_id <- gene_types %>% dplyr::filter(name == "immunomodulator") %>% .[["id"]]
io_target_id <- gene_types %>% dplyr::filter(name == "io_target") %>% .[["id"]]

# Make these data frames smaller so they're much faster.
driver_mutations <- driver_mutations %>%
  dplyr::distinct(gene) %>%
  tibble::add_column(type_id = driver_mutation_id %>% as.integer)
immunomodulator_expr <- immunomodulator_expr %>%
  dplyr::distinct(gene) %>%
  tibble::add_column(type_id = immunomodulator_id %>% as.integer)
io_target_expr <- io_target_expr %>%
  dplyr::distinct(gene) %>%
  tibble::add_column(type_id = io_target_id %>% as.integer)
genes_to_types <- driver_mutations %>%
  dplyr::bind_rows(immunomodulator_expr, io_target_expr) %>%
  dplyr::inner_join(genes, by = c("gene" = "hgnc")) %>%
  dplyr::distinct(id, type_id) %>%
  dplyr::rename_at("id", ~("gene_id"))
cat(crayon::blue("Build genes_to_types data."), fill = TRUE)

cat(crayon::magenta("Building genes_to_types table."), fill = TRUE)
table_written <- genes_to_types %>% .GlobalEnv$write_table_ts("genes_to_types")
cat(crayon::blue("Built genes_to_types table. (", nrow(genes_to_types), "rows )"), fill = TRUE, sep = " ")

# Clean up.
rm(driver_mutations)
rm(driver_mutation_id)
rm(immunomodulator_expr)
rm(immunomodulator_id)
rm(io_target_expr)
rm(io_target_id)
rm(genes)
rm(gene_types)
rm(genes_to_types)
rm(table_written)
cat("Cleaned up.", fill = TRUE)
gc()
