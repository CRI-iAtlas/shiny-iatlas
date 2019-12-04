# Build gene_types
gene_types <- dplyr::tibble(
  name = c("immunomodulator", "io_target", "driver_mutation"),
  display = c("Immunomodulator", "IO Target", "Driver Mutation")
) %>%
  .GlobalEnv$write_table_ts(.GlobalEnv$con, "gene_types", .)
