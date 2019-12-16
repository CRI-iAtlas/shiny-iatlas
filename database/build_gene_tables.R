cat(crayon::magenta("Importing driver mutation feather files for genes"), fill = TRUE)
driver_mutations1 <- feather::read_feather("../data2/driver_mutations1.feather")
driver_mutations2 <- feather::read_feather("../data2/driver_mutations2.feather")
driver_mutations3 <- feather::read_feather("../data2/driver_mutations3.feather")
driver_mutations4 <- feather::read_feather("../data2/driver_mutations4.feather")
driver_mutations5 <- feather::read_feather("../data2/driver_mutations5.feather")
driver_mutations <- dplyr::bind_rows(
  driver_mutations1,
  driver_mutations2,
  driver_mutations3,
  driver_mutations4,
  driver_mutations5
)
cat(crayon::blue("Imported driver mutation feather files for genes"), fill = TRUE)

# Clean up.
rm(driver_mutations1)
rm(driver_mutations2)
rm(driver_mutations3)
rm(driver_mutations4)
rm(driver_mutations5)
cat("Cleaned up.", fill = TRUE)
gc()

cat(crayon::magenta("Importing immunomodulators and io_target feather files for genes"), fill = TRUE)
immunomodulator_expr <- feather::read_feather("../data2/immunomodulator_expr.feather")
immunomodulators <-
  feather::read_feather("../data2/immunomodulators.feather") %>%
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
io_target_expr1 <- feather::read_feather("../data2/io_target_expr1.feather")
io_target_expr2 <- feather::read_feather("../data2/io_target_expr2.feather")
io_target_expr3 <- feather::read_feather("../data2/io_target_expr3.feather")
io_target_expr4 <- feather::read_feather("../data2/io_target_expr4.feather")
io_target_expr <- dplyr::bind_rows(
  io_target_expr1,
  io_target_expr2,
  io_target_expr3,
  io_target_expr4
)
cat(crayon::blue("Imported immunomodulators and io_target feather files for genes"), fill = TRUE)

# Clean up.
rm(io_target_expr1)
rm(io_target_expr2)
rm(io_target_expr3)
rm(io_target_expr4)
cat("Cleaned up.", fill = TRUE)
gc()

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

# Compbine all the gene data.
all_genes_01 <-
  dplyr::bind_rows(
    driver_mutations,
    immunomodulator_expr,
    io_target_expr
  ) %>%
  dplyr::select(gene) %>%
  dplyr::distinct(gene, .keep_all = TRUE)
cat(crayon::blue("Bound all_genes_01"), fill = TRUE)

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

all_genes_02 <- immunomodulators %>% dplyr::bind_rows(io_targets)
cat(crayon::blue("Bound all_genes_02"), fill = TRUE)

all_genes_01 <- all_genes_01 %>% dplyr::anti_join(all_genes_02, by = "gene")
cat(crayon::blue("Joind genes"), fill = TRUE)

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
cat(crayon::blue("Bound all_genes"), fill = TRUE)

# Clean up.
rm(immunomodulators)
rm(io_targets)
rm(all_genes_01)
rm(all_genes_02)
cat("Cleaned up.", fill = TRUE)
gc()

cat(crayon::magenta("Building gene_types data"), fill = TRUE)
gene_types <- dplyr::tibble(
  name = c("immunomodulator", "io_target", "driver_mutation"),
  display = c("Immunomodulator", "IO Target", "Driver Mutation")
)
cat(crayon::magenta("Building gene_types table."), fill = TRUE)
table_written <- gene_types %>% .GlobalEnv$write_table_ts("gene_types")
gene_types <- .GlobalEnv$read_table("gene_types")
cat(crayon::blue("Built gene_types table."), fill = TRUE)

cat(crayon::magenta("Building gene_types data"), fill = TRUE)
super_categories <- all_genes %>% .GlobalEnv$rebuild_gene_relational_data("super_category", "name")
cat(crayon::magenta("Building gene_types table."), fill = TRUE)
table_written <- super_categories %>% .GlobalEnv$write_table_ts("super_categories")
super_categories <- .GlobalEnv$read_table("super_categories")
cat(crayon::blue("Built super_categories table."), fill = TRUE)

cat(crayon::magenta("Building gene_families data"), fill = TRUE)
gene_families <- all_genes %>% .GlobalEnv$rebuild_gene_relational_data("gene_family", "name")
cat(crayon::magenta("Building gene_families table."), fill = TRUE)
table_written <- gene_families %>% .GlobalEnv$write_table_ts("gene_families")
gene_families <- .GlobalEnv$read_table("gene_families")
cat(crayon::blue("Built gene_families table."), fill = TRUE)

cat(crayon::magenta("Building immune_checkpoints data"), fill = TRUE)
immune_checkpoints <- all_genes %>% .GlobalEnv$rebuild_gene_relational_data("immune_checkpoint", "name")
cat(crayon::magenta("Building immune_checkpoints table."), fill = TRUE)
table_written <- immune_checkpoints %>% .GlobalEnv$write_table_ts("immune_checkpoints")
immune_checkpoints <- .GlobalEnv$read_table("immune_checkpoints")
cat(crayon::blue("Built immune_checkpoints table."), fill = TRUE)

cat(crayon::magenta("Building gene_functions data"), fill = TRUE)
gene_functions <- all_genes %>% .GlobalEnv$rebuild_gene_relational_data("gene_function", "name")
cat(crayon::magenta("Building gene_functions table."), fill = TRUE)
table_written <- gene_functions %>% .GlobalEnv$write_table_ts("gene_functions")
gene_functions <- .GlobalEnv$read_table("gene_functions")
cat(crayon::blue("Built gene_functions table."), fill = TRUE)

cat(crayon::magenta("Built pathways data"), fill = TRUE)
pathways <- all_genes %>% .GlobalEnv$rebuild_gene_relational_data("pathway", "name")
cat(crayon::magenta("Built pathways table."), fill = TRUE)
table_written <- pathways %>% .GlobalEnv$write_table_ts("pathways")
pathways <- .GlobalEnv$read_table("pathways")
cat(crayon::blue("Built pathways table."), fill = TRUE)

cat(crayon::magenta("Built therapy_types data"), fill = TRUE)
therapy_types <- all_genes %>% .GlobalEnv$rebuild_gene_relational_data("therapy_type", "name")
cat(crayon::magenta("Built therapy_types table."), fill = TRUE)
table_written <- therapy_types %>% .GlobalEnv$write_table_ts("therapy_types")
therapy_types <- .GlobalEnv$read_table("therapy_types")
cat(crayon::blue("Built therapy_types table."), fill = TRUE)

cat(crayon::magenta("Building genes data"), fill = TRUE)
genes <- all_genes %>%
  .GlobalEnv$rebuild_genes(
    super_categories,
    gene_families,
    immune_checkpoints,
    gene_functions,
    pathways,
    therapy_types) %>%
  dplyr::select(-c("link")) %>%
  dplyr::select(-c("type")) %>%
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
cat(crayon::magenta("Building genes table."), fill = TRUE)
table_written <- genes %>% .GlobalEnv$write_table_ts("genes")
genes <- .GlobalEnv$read_table("genes")
cat(crayon::blue("Built genes table."), fill = TRUE)

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

driver_mutation_row <- gene_types %>% dplyr::filter(name == "driver_mutation")
driver_mutation_id <- driver_mutation_row[["id"]]
immunomodulator_row <- gene_types %>% dplyr::filter(name == "immunomodulator")
immunomodulator_id <- immunomodulator_row[["id"]]
io_target_row <- gene_types %>% dplyr::filter(name == "io_target")
io_target_id <- io_target_row[["id"]]

# Make these data frames smaller so they're much faster.
driver_mutations <- driver_mutations %>% dplyr::distinct(gene) %>% as.list
immunomodulator_expr <- immunomodulator_expr %>% dplyr::distinct(gene) %>% as.list
io_target_expr <- io_target_expr %>% dplyr::distinct(gene) %>% as.list

# Hash these data frames so they're much faster.
driver_mutations <- driver_mutations$gene %>% hash::hash(TRUE)
immunomodulator_expr <- immunomodulator_expr$gene %>% hash::hash(TRUE)
io_target_expr <- io_target_expr$gene %>% hash::hash(TRUE)

cat(crayon::magenta("Building genes_to_types."), fill = TRUE)
genes_to_types <- dplyr::tibble() %>%
  tibble::add_column(gene_id = NA %>% as.integer, type_id = NA %>% as.integer)
genes_length <- length(genes$hgnc)
genes %>%
  dplyr::select(id:hgnc) %>%
  dplyr::rowwise() %>%
  dplyr::do(as.data.frame({
    current_hgnc <- .data$hgnc
    current_id <- .data$id
    row_number <- which(current_hgnc == genes$hgnc)

    svMisc::progress(row_number, genes_length - 1, progress.bar = TRUE)

    if (hash::has.key(c(current_hgnc), driver_mutations)) {
      genes_to_types <<- genes_to_types %>% dplyr::add_row(gene_id = current_id, type_id = driver_mutation_id)
    }

    if (hash::has.key(c(current_hgnc), immunomodulator_expr)) {
      genes_to_types <<- genes_to_types %>% dplyr::add_row(gene_id = current_id, type_id = immunomodulator_id)
    }

    if (hash::has.key(c(current_hgnc), io_target_expr)) {
      genes_to_types <<- genes_to_types %>% dplyr::add_row(gene_id = current_id, type_id = io_target_id)
    }

    if (row_number == genes_length) {
      cat(crayon::blue("Build genes_to_types."), fill = TRUE)
    }

    rm(current_hgnc)
    rm(current_id)
    rm(row_number)
  }))

table_written <- genes_to_types %>% .GlobalEnv$write_table_ts("genes_to_types")

# Clean up.
rm(driver_mutations)
rm(driver_mutation_row)
rm(driver_mutation_id)
rm(immunomodulator_expr)
rm(immunomodulator_row)
rm(immunomodulator_id)
rm(io_target_expr)
rm(io_target_row)
rm(io_target_id)
rm(genes)
rm(genes_length)
rm(gene_types)
rm(genes_to_types)
rm(table_written)
cat("Cleaned up.", fill = TRUE)
gc()
