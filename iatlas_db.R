# Ensure magrittr is installed.
if (!'magrittr' %in% installed.packages()) {
  install.packages("magrittr")
}

# Ensure RPostgres is installed.
if (!'RPostgres' %in% installed.packages()) {
  install.packages("RPostgres")
}

# Load magrittr so %>% is available
library("magrittr")

# Loading RPostgres loads DBI automatically
library("RPostgres")

# build_iatlas_db <- function(env = "dev", reset = "reset") {

# Make the create_db function available.
source("database/create_db.R")

# Reset the database so new data is not corrupted by any old data.
.GlobalEnv$create_db("dev", "reset")

# Make the custom database functions available.
source("database/database_functions.R")

# The database connection.
source("database/connect_to_db.R")

# Create a global variable to hold the connection.
con <- .GlobalEnv$connect_to_db()

source("database/build_features_tables.R")

source("database/build_groups_tables.R")

source("database/build_gene_tables.R")

# Close the database connection.
RPostgres::dbDisconnect(.GlobalEnv$con)

### Clean up ###
# Data
rm(con)

# Functions
rm(connect_to_db)
rm(create_db)
rm(delete_rows)
rm(rebuild_features)
rm(rebuild_groups)
rm(value_to_id)
rm(write_table_ts)
# }
