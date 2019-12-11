# Get data from feather files as data.frames and convert them to a tibbles.
driver_mutations1 <-
  feather::read_feather("../data2/driver_mutations1.feather")
driver_mutations2 <-
  feather::read_feather("../data2/driver_mutations2.feather")
driver_mutations3 <-
  feather::read_feather("../data2/driver_mutations3.feather")
driver_mutations4 <-
  feather::read_feather("../data2/driver_mutations4.feather")
driver_mutations5 <-
  feather::read_feather("../data2/driver_mutations5.feather")
immunomodulator_expr <-
  feather::read_feather("../data2/immunomodulator_expr.feather")
immunomodulators <-
  feather::read_feather("../data2/immunomodulators.feather") %>%
  dplyr::filter(!is.na(gene)) %>%
  dplyr::mutate_at(dplyr::vars(entrez), as.numeric) %>%
  tibble::add_column(
    pathway = NA %>% as.character,
    therapy_type = NA %>% as.character,
    description = NA %>% as.character,
    link = NA %>% as.character
  )
io_target_expr1 <-
  feather::read_feather("../data2/io_target_expr1.feather")
io_target_expr2 <-
  feather::read_feather("../data2/io_target_expr2.feather")
io_target_expr3 <-
  feather::read_feather("../data2/io_target_expr3.feather")
io_target_expr4 <-
  feather::read_feather("../data2/io_target_expr4.feather")
io_targets <-
  feather::read_feather("../data2/io_targets.feather") %>%
  dplyr::filter(!is.na(gene)) %>%
  dplyr::distinct(gene, .keep_all = TRUE) %>%
  dplyr::mutate_at(dplyr::vars(entrez), as.numeric) %>%
  tibble::add_column(
    gene_family = NA %>% as.character,
    super_category = NA %>% as.character,
    immune_checkpoint = NA %>% as.character,
    gene_function = NA %>% as.character,
    reference = NA %>% as.character
  )

cat("Imported feather files for genes", fill = TRUE)

# Compbine all the gene data.
all_genes_01 <-
  dplyr::bind_rows(
    driver_mutations1,
    driver_mutations2,
    driver_mutations3,
    driver_mutations4,
    driver_mutations5,
    immunomodulator_expr,
    io_target_expr1,
    io_target_expr2,
    io_target_expr3,
    io_target_expr4
  ) %>%
  dplyr::select(gene) %>%
  dplyr::distinct(gene, .keep_all = TRUE)

cat("Bound all_genes_01", fill = TRUE)

# Clean up.
rm(driver_mutations1)
rm(driver_mutations2)
rm(driver_mutations3)
rm(driver_mutations4)
rm(driver_mutations5)
rm(immunomodulator_expr)
rm(io_target_expr1)
rm(io_target_expr2)
rm(io_target_expr3)
rm(io_target_expr4)

cat("Cleaned up.", fill = TRUE)
gc()

io_targets <- io_targets %>%
  dplyr::select(dplyr::everything()) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(gene_family = .GlobalEnv$switch_value(.data, "gene", "gene_family", immunomodulators),
    super_category = .GlobalEnv$switch_value(.data, "gene", "super_category", immunomodulators),
    immune_checkpoint = .GlobalEnv$switch_value(.data, "gene", "immune_checkpoint", immunomodulators),
    gene_function = .GlobalEnv$switch_value(.data, "gene", "gene_function", immunomodulators),
    reference = .GlobalEnv$switch_value(.data, "gene", "reference", immunomodulators)
  )

immunomodulators <- immunomodulators %>% dplyr::anti_join(io_targets, by = "gene")

all_genes_02 <- immunomodulators %>% dplyr::bind_rows(io_targets)

cat("Bound all_genes_02", fill = TRUE)

all_genes_01 <- all_genes_01 %>% dplyr::anti_join(all_genes_02, by = "gene")

cat("Joind genes", fill = TRUE)

all_genes <- all_genes_02 %>% 
  dplyr::bind_rows(all_genes_01) %>%
  dplyr::as_tibble() %>%
  dplyr::rename_at("gene", ~("hgnc")) %>%
  tibble::add_column(sc_int = NA, .after = "hgnc") %>%
  tibble::add_column(gene_family_int = NA, .after = "hgnc") %>%
  tibble::add_column(immune_checkpoint_int = NA, .after = "hgnc") %>%
  tibble::add_column(gene_function_int = NA, .after = "hgnc") %>%
  tibble::add_column(pathway_int = NA, .after = "hgnc") %>%
  tibble::add_column(therapy_type_int = NA, .after = "hgnc") %>%
  dplyr::arrange(hgnc)

cat("Bound all_genes", fill = TRUE)

# Clean up.
rm(immunomodulators)
rm(io_targets)
rm(all_genes_01)
rm(all_genes_02)

cat("Cleaned up.", fill = TRUE)
gc()

# Build gene_types table.
gene_types <- dplyr::tibble(
  name = c("immunomodulator", "io_target", "driver_mutation"),
  display = c("Immunomodulator", "IO Target", "Driver Mutation")
)
gene_types %>% .GlobalEnv$write_table_ts(.GlobalEnv$con, "gene_types", .)

cat("Built gene_types table.", fill = TRUE)

super_categories <- all_genes %>% .GlobalEnv$rebuild_gene_relational_data("super_category", "name")
super_categories %>% .GlobalEnv$write_table_ts(.GlobalEnv$con, "super_categories", .)
super_categories <- RPostgres::dbReadTable(.GlobalEnv$con, "super_categories")

cat("Built super_categories table.", fill = TRUE)

gene_families <- all_genes %>% .GlobalEnv$rebuild_gene_relational_data("gene_family", "name")
gene_families %>% .GlobalEnv$write_table_ts(.GlobalEnv$con, "gene_families", .)
gene_families <- RPostgres::dbReadTable(.GlobalEnv$con, "gene_families")

cat("Built gene_families table.", fill = TRUE)

immune_checkpoints <- all_genes %>% .GlobalEnv$rebuild_gene_relational_data("immune_checkpoint", "name")
immune_checkpoints %>% .GlobalEnv$write_table_ts(.GlobalEnv$con, "immune_checkpoints", .)
immune_checkpoints <- RPostgres::dbReadTable(.GlobalEnv$con, "immune_checkpoints")

cat("Built immune_checkpoints table.", fill = TRUE)

gene_functions <- all_genes %>% .GlobalEnv$rebuild_gene_relational_data("gene_function", "name")
gene_functions %>% .GlobalEnv$write_table_ts(.GlobalEnv$con, "gene_functions", .)
gene_functions <- RPostgres::dbReadTable(.GlobalEnv$con, "gene_functions")

cat("Built gene_functions table.", fill = TRUE)

pathways <- all_genes %>% .GlobalEnv$rebuild_gene_relational_data("pathway", "name")
pathways %>% .GlobalEnv$write_table_ts(.GlobalEnv$con, "pathways", .)
pathways <- RPostgres::dbReadTable(.GlobalEnv$con, "pathways")

cat("Built pathways table.", fill = TRUE)

therapy_types <- all_genes %>% .GlobalEnv$rebuild_gene_relational_data("therapy_type", "name")
therapy_types %>% .GlobalEnv$write_table_ts(.GlobalEnv$con, "therapy_types", .)
therapy_types <- RPostgres::dbReadTable(.GlobalEnv$con, "therapy_types")

cat("Built therapy_types table.", fill = TRUE)

genes <- all_genes %>%
  .GlobalEnv$rebuild_genes(
    super_categories,
    gene_families,
    immune_checkpoints,
    gene_functions,
    pathways,
    therapy_types) %>%
  dplyr::select(-c("display2")) %>%
  dplyr::select(-c("link")) %>%
  dplyr::select(-c("reference")) %>%
  dplyr::select(-c("super_category")) %>%
  dplyr::rename_at("sc_int", ~("super_cat_id")) %>%
  dplyr::select(-c("gene_family")) %>%
  dplyr::rename_at("gene_family_int", ~("family_id")) %>%
  dplyr::select(-c("immune_checkpoint")) %>%
  dplyr::rename_at("immune_checkpoint_int", ~("immune_checkpoint_id")) %>%
  dplyr::select(-c("gene_function")) %>%
  dplyr::rename_at("gene_function_int", ~("function_id")) %>%
  dplyr::select(-c("pathway")) %>%
  dplyr::rename_at("pathway_int", ~("pathway_id")) %>%
  dplyr::select(-c("therapy_type")) %>%
  dplyr::rename_at("therapy_type_int", ~("therapy_type_id"))
genes %>% .GlobalEnv$write_table_ts(.GlobalEnv$con, "genes", .)
# genes <- RPostgres::dbReadTable(.GlobalEnv$con, "genes")
# 
# cat("Built genes table.", fill = TRUE)
# 
# genes_to_types <- dplyr::tibble() %>% 
#   tibble::add_column(gene_id = NA, type_id = NA) %>%
#   as.data.frame
# for (row in 1:nrow(gene_types)) {
#   current_genes <- genes %>%
#     dplyr::filter(hgnc == gene_types[row, "name"]) %>%
#     as.data.frame
#   for (gene_row in 1:nrow(current_genes)) {
#     genes_to_types <- genes_to_types %>%
#       dplyr::add_row(gene_id = current_genes[gene_row, "id"], type_id = gene_types[row, "id"])
#   }
# }

# Clean up.
rm(super_categories)
rm(gene_families)
rm(immune_checkpoints)
rm(gene_functions)
rm(pathways)
rm(therapy_types)
# rm(genes_to_types)
# rm(all_genes)

cat("Cleaned up.", fill = TRUE)
gc()
