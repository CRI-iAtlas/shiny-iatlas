# Get data from feather files as data.frames and convert them to a tibbles.
# driver_mutations1 <-
#   feather::read_feather("data2/driver_mutations1.feather")
# driver_mutations2 <-
#   feather::read_feather("data2/driver_mutations2.feather")
# driver_mutations3 <-
#   feather::read_feather("data2/driver_mutations3.feather")
# driver_mutations4 <-
#   feather::read_feather("data2/driver_mutations4.feather")
# driver_mutations5 <-
#   feather::read_feather("data2/driver_mutations5.feather")
# immunomodulator_expr <-
#   feather::read_feather("data2/immunomodulator_expr.feather")
# immunomodulators <-
#   feather::read_feather("data2/immunomodulators.feather")
# io_target_expr1 <-
#   feather::read_feather("data2/io_target_expr1.feather")
# io_target_expr2 <-
#   feather::read_feather("data2/io_target_expr2.feather")
# io_target_expr3 <-
#   feather::read_feather("data2/io_target_expr3.feather")
# io_target_expr4 <-
#   feather::read_feather("data2/io_target_expr4.feather")
# io_targets <-
#   feather::read_feather("data2/io_targets.feather")
# 
# cat("Loaded Feather Files", fill = TRUE)
# 
# # Compbine all the gene data.
# all_genes_01 <-
#   dplyr::bind_rows(
#     driver_mutations1,
#     driver_mutations2,
#     driver_mutations3,
#     driver_mutations4,
#     driver_mutations5,
#     immunomodulator_expr,
#     io_target_expr1,
#     io_target_expr2,
#     io_target_expr3,
#     io_target_expr4
#   ) %>%
#   dplyr::select(gene)
# 
# cat("Bound all_genes_01", fill = TRUE)
# 
# all_genes_02 <- dplyr::bind_rows(immunomodulators, io_targets)
# 
# cat("Bound all_genes_02", fill = TRUE)
# 
# all_genes <- dplyr::bind_rows(all_genes_02, all_genes_01) %>%
#   dplyr::distinct(gene) %>%
#   dplyr::arrange(gene)
# 
# cat("Bound all_genes", fill = TRUE)
# 
# # Clean up.
# rm(driver_mutations1)
# rm(driver_mutations2)
# rm(driver_mutations3)
# rm(driver_mutations4)
# rm(driver_mutations5)
# rm(feature_values_long)
# rm(immunomodulator_expr)
# rm(immunomodulators)
# rm(io_target_expr1)
# rm(io_target_expr2)
# rm(io_target_expr3)
# rm(io_target_expr4)
# rm(io_targets)
# rm(all_genes_01)
# rm(all_genes_02)

# cat("Cleaned!", fill = TRUE)

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

# cat("Go for the loop!", fill = TRUE)
# 
# for (row in 1:nrow(all_genes)) {
#   super_categories <- all_genes %>% .GlobalEnv$rebuild_gene_relational_data("super_category")
#   
#   gene_families <- all_genes %>% .GlobalEnv$rebuild_gene_relational_data("gene_family")
#   
#   # entrez <- all_genes %>% .GlobalEnv$rebuild_gene_relational_data("entrez", "value")
#   
#   immune_checkpoints <- all_genes %>% .GlobalEnv$rebuild_gene_relational_data("immune_checkpoint")
#   
#   gene_functions <- all_genes %>% .GlobalEnv$rebuild_gene_relational_data("gene_function")
#   
#   pathways <- all_genes %>% .GlobalEnv$rebuild_gene_relational_data("pathway")
#   
#   therapy_types <- all_genes %>% .GlobalEnv$rebuild_gene_relational_data("therapy_type")
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
