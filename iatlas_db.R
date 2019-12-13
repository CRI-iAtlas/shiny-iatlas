# Ensure svMisc is installed.
if (!'svMisc' %in% installed.packages()) {
  install.packages("svMisc")
}

# Ensure hash is installed.
if (!'hash' %in% installed.packages()) {
  install.packages("hash")
}

# Ensure magrittr is installed.
if (!'magrittr' %in% installed.packages()) {
  install.packages("magrittr")
}

# Ensure RPostgres is installed.
if (!'RPostgres' %in% installed.packages()) {
  install.packages("RPostgres")
}

# Loading svMisc.
require("svMisc")

# Loading hash
library("hash")

# Load magrittr so %>% is available.
library("magrittr")

# Loading RPostgres loads DBI automatically.
library("RPostgres")

# build_iatlas_db <- function(env = "dev", reset = "reset") {

# Make the create_db function available.
source("database/create_db.R", chdir = TRUE)

# Reset the database so new data is not corrupted by any old data.
.GlobalEnv$create_db("dev", "reset")

# Show garbage collection info
# gcinfo(TRUE)

# Make the custom data functions available.
source("database/data_functions.R", chdir = TRUE)

# Make the custom database functions available.
source("database/database_functions.R", chdir = TRUE)

# The database connection.
source("database/connect_to_db.R", chdir = TRUE)

# Create a global variable to hold the connection.
con <- .GlobalEnv$connect_to_db()

cat("Created DB cponnection.", fill = TRUE)

source("database/build_features_tables.R", chdir = TRUE)

source("database/build_tags_tables.R", chdir = TRUE)

source("database/build_samples_tables.R", chdir = TRUE)

source("database/build_gene_tables.R", chdir = TRUE)

# Close the database connection.
RPostgres::dbDisconnect(.GlobalEnv$con)

cat("Closed DB cponnection.", fill = TRUE)

### Clean up ###
# Data
rm(con)

# Functions
rm(connect_to_db)
rm(create_db)
rm(delete_rows)
rm(rebuild_features)
rm(update_features_to_samples)
rm(rebuild_features_to_samples)
rm(get_ids_from_heirarchy)
rm(build_tag_id_data)
rm(rebuild_samples_to_tags)
rm(rebuild_gene_relational_data)
rm(rebuild_genes)
rm(rebuild_tags)
rm(switch_value)
rm(value_to_id)
rm(write_table_ts)
rm(is_df_empty)
rm(link_to_references)

cat("Cleaned up.", fill = TRUE)
gc()

# Don't show garbage collection details any longer.
# gcinfo(FALSE)
# }
