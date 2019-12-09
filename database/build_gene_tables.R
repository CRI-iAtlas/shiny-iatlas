# Get data from feather files as data.frames and convert them to a tibbles.
driver_mutations1 <-
  feather::read_feather("data2/driver_mutations1.feather")
driver_mutations2 <-
  feather::read_feather("data2/driver_mutations2.feather")
driver_mutations3 <-
  feather::read_feather("data2/driver_mutations3.feather")
driver_mutations4 <-
  feather::read_feather("data2/driver_mutations4.feather")
driver_mutations5 <-
  feather::read_feather("data2/driver_mutations5.feather")
immunomodulator_expr <-
  feather::read_feather("data2/immunomodulator_expr.feather")
immunomodulators <-
  feather::read_feather("data2/immunomodulators.feather") %>%
  dplyr::mutate_at(dplyr::vars(entrez), as.numeric) %>%
  tibble::add_column(
    pathway = NA %>% as.character,
    therapy_type = NA %>% as.character,
    description = NA %>% as.character,
    link = NA %>% as.character
  )
io_target_expr1 <-
  feather::read_feather("data2/io_target_expr1.feather")
io_target_expr2 <-
  feather::read_feather("data2/io_target_expr2.feather")
io_target_expr3 <-
  feather::read_feather("data2/io_target_expr3.feather")
io_target_expr4 <-
  feather::read_feather("data2/io_target_expr4.feather")
io_targets <-
  feather::read_feather("data2/io_targets.feather") %>%
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
  dplyr::select(gene)

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

# fix_the_value <- function(reference, current_value, object) {
#   if (reference %in% object) {
#     currect_reference_row
#   }
# }

immunomodulators <- immunomodulators %>% as.data.frame
for (row in 1:nrow(immunomodulators)) {
  # svMisc::progress(row, nrow(immunomodulators) - 1, progress.bar = TRUE)
  
  immunomodulator_gene <- immunomodulators[row, "gene"]
  cat("immunomodulators gene:", immunomodulator_gene, fill = TRUE, sep = " ")
  io_targets <- io_targets %>%
    dplyr::select(dplyr::everything()) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      gene_family = .GlobalEnv$value_to_id(gene, gene_family, immunomodulators[row, "gene_family"], immunomodulator_gene),
      super_category = .GlobalEnv$value_to_id(gene, super_category, immunomodulators[row, "super_category"], immunomodulator_gene),
      immune_checkpoint = .GlobalEnv$value_to_id(gene, immune_checkpoint, immunomodulators[row, "immune_checkpoint"], immunomodulator_gene),
      gene_function = .GlobalEnv$value_to_id(gene, gene_function, immunomodulators[row, "gene_function"], immunomodulator_gene),
      reference = .GlobalEnv$value_to_id(gene, reference, immunomodulators[row, "reference"], immunomodulator_gene)
    )
  rm(immunomodulator_gene)
  gc()
  
  # if (row == nrow(immunomodulators)) {
  #   cat("Rebuilt io_targets", fill = TRUE)
  # }
}
# for (row in 1:nrow(immunomodulators)) {
#   immunomodulator_gene <- immunomodulators[row, "gene"]
#   if (immunomodulator_gene %in% io_targets$gene) {
#     immunomodulators <- immunomodulators %>% dplyr::filter(gene != immunomodulator_gene)
#   }
#   rm(immunomodulator_gene)
#   gc()
# }
immunomodulators <- dplyr::as_tibble(immunomodulators) %>% dplyr::filter(!gene %in% io_targets$gene)

io_targets <- io_targets %>% as.data.frame
# for (row in 1:nrow(io_targets)) {
#   # svMisc::progress(row, nrow(io_targets) - 1, progress.bar = TRUE)
#   
#   io_targets_gene <- io_targets[row, "gene"]
#   cat("io_targets gene:", io_targets_gene, fill = TRUE, sep = " ")
#   immunomodulators <- immunomodulators %>%
#     dplyr::select(dplyr::everything()) %>%
#     dplyr::rowwise() %>%
#     dplyr::mutate(
#       pathway = .GlobalEnv$value_to_id(gene, pathway, immunomodulators[row, "pathway"], io_targets_gene),
#       therapy_type = .GlobalEnv$value_to_id(gene, therapy_type, immunomodulators[row, "therapy_type"], io_targets_gene),
#       description = .GlobalEnv$value_to_id(gene, description, immunomodulators[row, "description"], io_targets_gene),
#       link = .GlobalEnv$value_to_id(gene, link, immunomodulators[row, "link"], io_targets_gene)
#     )
#   rm(io_targets_gene)
#   gc()
#   
#   # if (row == nrow(io_targets)) {
#   #   cat("Rebuilt immunomodulators", fill = TRUE)
#   # }
# }
io_targets <- dplyr::as_tibble(io_targets)

all_genes_02 <- dplyr::bind_rows(immunomodulators, io_targets)

cat("Bound all_genes_02", fill = TRUE)

all_genes <- dplyr::bind_rows(all_genes_02, all_genes_01) %>%
  dplyr::distinct(gene, .keep_all = TRUE) %>%
  dplyr::arrange(gene)

cat("Bound all_genes", fill = TRUE)

# Clean up.
# rm(immunomodulators)
# rm(io_targets)
rm(all_genes_01)
# rm(all_genes_02)

cat("Cleaned up.", fill = TRUE)
gc()

# Build gene_types table.
gene_types <- dplyr::tibble(
  name = c("immunomodulator", "io_target", "driver_mutation"),
  display = c("Immunomodulator", "IO Target", "Driver Mutation")
)
gene_types %>% .GlobalEnv$write_table_ts(.GlobalEnv$con, "gene_types", .)

rm(gene_types)

cat("Built gene_types table.", fill = TRUE)

# Build super_categories data.
# super_categories <- dplyr::tibble() %>%
#   tibble::add_column(name = NA)
# 
# # Build gene_families data.
# gene_families <- dplyr::tibble() %>%
#   tibble::add_column(name = NA)
# 
# # Build entrez data.
# entrez <- dplyr::tibble() %>%
#   tibble::add_column(name = NA)
# 
# # Build immune_checkpoints data.
# immune_checkpoints <- dplyr::tibble() %>%
#   tibble::add_column(name = NA)
# 
# # Build gene_functions data.
# gene_functions <- dplyr::tibble() %>%
#   tibble::add_column(name = NA)
# 
# # Build pathways data.
# pathways <- dplyr::tibble() %>%
#   tibble::add_column(name = NA)
# 
# # Build therapy_types data.
# therapy_types <- dplyr::tibble() %>%
#   tibble::add_column(name = NA)

cat("Go for the loop -", nrow(all_genes), "rows", fill = TRUE, sep = " ")

# for (row in 1:nrow(all_genes)) {
#   svMisc::progress(row, nrow(all_genes) - 1, progress.bar = TRUE)
#   
#   super_categories <- all_genes %>% .GlobalEnv$rebuild_gene_relational_data("super_category")
# 
#   gene_families <- all_genes %>% .GlobalEnv$rebuild_gene_relational_data("gene_family")
# 
#   entrez_data <- all_genes %>% .GlobalEnv$rebuild_gene_relational_data("entrez", "value")
# 
#   immune_checkpoints <- all_genes %>% .GlobalEnv$rebuild_gene_relational_data("immune_checkpoint")
# 
#   gene_functions <- all_genes %>% .GlobalEnv$rebuild_gene_relational_data("gene_function")
# 
#   pathways <- all_genes %>% .GlobalEnv$rebuild_gene_relational_data("pathway")
# 
#   therapy_types <- all_genes %>% .GlobalEnv$rebuild_gene_relational_data("therapy_type")
#   
#   if (row == nrow(all_genes)) {
#     cat("Rebuilt super_categories,gene_families, entrez_data, immune_checkpoints, gene_functions, pathways, and therapy_types", fill = TRUE)
#   }
# 
#   # super_categories <- super_categories %>%
#   #   dplyr::select(everything()) %>%
#   #   dplyr::rowwise() %>%
#   #   dplyr::add_row(name = all_genes[row, "super_category"])
#   #
#   # gene_families <- gene_families %>%
#   #   dplyr::select(everything()) %>%
#   #   dplyr::rowwise() %>%
#   #   dplyr::add_row(name = all_genes[row, "gene_family"])
#   #
#   # entrez <- entrez %>%
#   #   dplyr::select(everything()) %>%
#   #   dplyr::rowwise() %>%
#   #   dplyr::add_row(name = all_genes[row, "entrez"])
#   #
#   # immune_checkpoints <- immune_checkpoints %>%
#   #   dplyr::select(everything()) %>%
#   #   dplyr::rowwise() %>%
#   #   dplyr::add_row(name = all_genes[row, "immune_checkpoint"])
#   #
#   # gene_functions <- gene_functions %>%
#   #   dplyr::select(everything()) %>%
#   #   dplyr::rowwise() %>%
#   #   dplyr::add_row(name = all_genes[row, "gene_function"])
#   #
#   # pathways <- pathways %>%
#   #   dplyr::select(everything()) %>%
#   #   dplyr::rowwise() %>%
#   #   dplyr::add_row(name = all_genes[row, "pathway"])
#   #
#   # therapy_types <- therapy_types %>%
#   #   dplyr::select(everything()) %>%
#   #   dplyr::rowwise() %>%
#   #   dplyr::add_row(name = all_genes[row, "therapy_type"])
# }

gc()

