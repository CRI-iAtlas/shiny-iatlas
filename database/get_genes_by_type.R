source("database/load_dependencies.R")

.GlobalEnv$load_dependencies()

rm(load_dependencies, pos = ".GlobalEnv")

# The database connection.
source("database/connect_to_db.R", chdir = TRUE)

# Create a global variable to hold the pool DB connection.
.GlobalEnv$pool <- .GlobalEnv$connect_to_db()

cat(crayon::green("Created DB connection."), fill = TRUE)

get_genes_by_type <- function(type) {
  current_pool <- pool::poolCheckout(.GlobalEnv$pool)

  genes <- current_pool %>%
    dplyr::tbl("genes") %>%
    dplyr::as_tibble() %>%
    dplyr::left_join(
      current_pool %>%
        dplyr::tbl("genes_to_types") %>%
        dplyr::as_tibble(),
      by = c("id" = "gene_id")
    ) %>%
    dplyr::left_join(
      current_pool %>%
        dplyr::tbl("gene_types") %>%
        dplyr::as_tibble(),
      by = c("type_id" = "id")
    ) %>%
    dplyr::rename_at("name", ~("type_name")) %>%
    dplyr::rename_at("display.x", ~("display")) %>%
    dplyr::filter(type_name == type) %>%
    dplyr::left_join(
      current_pool %>%
        dplyr::tbl("gene_families") %>%
        dplyr::as_tibble(),
      by = c("gene_family_id" = "id")
    ) %>%
    dplyr::rename_at("name", ~("gene_family")) %>%
    dplyr::left_join(
      current_pool %>%
        dplyr::tbl("gene_functions") %>%
        dplyr::as_tibble(),
      by = c("gene_function_id" = "id")
    ) %>%
    dplyr::rename_at("name", ~("gene_function")) %>%
    dplyr::left_join(
      current_pool %>%
        dplyr::tbl("immune_checkpoints") %>%
        dplyr::as_tibble(),
      by = c("immune_checkpoint_id" = "id")
    ) %>%
    dplyr::rename_at("name", ~("immune_checkpoint")) %>%
    dplyr::left_join(
      current_pool %>%
        dplyr::tbl("pathways") %>%
        dplyr::as_tibble(),
      by = c("pathway_id" = "id")
    ) %>%
    dplyr::rename_at("name", ~("pathway")) %>%
    dplyr::left_join(
      current_pool %>%
        dplyr::tbl("super_categories") %>%
        dplyr::as_tibble(),
      by = c("super_cat_id" = "id")
    ) %>%
    dplyr::rename_at("name", ~("super_category")) %>%
    dplyr::left_join(
      current_pool %>%
        dplyr::tbl("therapy_types") %>%
        dplyr::as_tibble(),
      by = c("therapy_type_id" = "id")
    ) %>%
    dplyr::rename_at("name", ~("therapy_type")) %>%
    dplyr::select(-c(id, type_id, gene_family_id, gene_function_id, immune_checkpoint_id, pathway_id, super_cat_id, therapy_type_id, display.y))

  pool::poolReturn(current_pool)

  return(genes)
}

"driver_mutation" %>%
  get_genes_by_type %>%
  feather::write_feather("data2/genes/driver_mutation_genes.feather")

"immunomodulator" %>%
  get_genes_by_type %>%
  feather::write_feather("data2/genes/immunomodulator_genes.feather")

"io_target" %>%
  get_genes_by_type %>%
  feather::write_feather("data2/genes/io_target_genes.feather")

# Close the database connection.
pool::poolClose(.GlobalEnv$pool)
cat(crayon::green("Closed DB connection."), fill = TRUE)

### Clean up ###
# Data
rm(pool, pos = ".GlobalEnv")
# rm(driver_mutation_genes)
# rm(immunomodulator_genes)
# rm(io_target_genes)

# Functions
rm(get_genes_by_type, pos = ".GlobalEnv")

cat("Cleaned up.", fill = TRUE)
gc()
