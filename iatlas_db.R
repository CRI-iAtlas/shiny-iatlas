# Ensure magrittr is installed.
if (!'magrittr' %in% installed.packages()) {
  install.packages("magrittr")
}

# Ensure RPostgres is installed.
if (!'RPostgres' %in% installed.packages()) {
  install.packages("RPostgres")
}

# Ensure svMisc is installed.
if (!'svMisc' %in% installed.packages()) {
  install.packages("svMisc")
}

# Load magrittr so %>% is available.
library("magrittr")

# Loading RPostgres loads DBI automatically.
library("RPostgres")

# Loading svMisc.
library("svMisc")

# build_iatlas_db <- function(env = "dev", reset = "reset") {

# Make the create_db function available.
source("database/create_db.R")

# Reset the database so new data is not corrupted by any old data.
.GlobalEnv$create_db("dev", "reset")

# Make the custom data functions available.
source("database/data_functions.R")

# Make the custom database functions available.
source("database/database_functions.R")

# The database connection.
source("database/connect_to_db.R")

# Create a global variable to hold the connection.
con <- .GlobalEnv$connect_to_db()

cat("Created DB cponnection.", fill = TRUE)

source("database/build_features_tables.R")

source("database/build_groups_tables.R")

source("database/build_samples_tables.R")

source("database/build_gene_tables.R")

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
rm(build_group_id_data)
rm(update_samples_to_groups)
rm(rebuild_samples_to_groups)
rm(rebuild_gene_relational_data)
rm(rebuild_groups)
rm(value_to_id)
rm(write_table_ts)
rm(is_df_empty)

cat("Cleaned up.", fill = TRUE)
gc(TRUE)
# }
