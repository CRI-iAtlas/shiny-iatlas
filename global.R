config_yaml <- yaml::read_yaml("configuration.yaml")
purrr::walk(config_yaml$libraries, library, character.only = T)
purrr::walk(config_yaml$function_files, source)

# general data loading & prep

DATAMODE <- "SQLite_db"
PANIMMUNE_DB   <- create_db2()
source("database/connect_to_db.R")
PANIMMUNE_DB2 <- connect_to_db()

create_conection <- function(table_name) {
    current_pool <- pool::poolCheckout(.GlobalEnv$PANIMMUNE_DB2)
    result <- dplyr::tbl(current_pool, table_name)
    pool::poolReturn(current_pool)
    return(result)
}

purrr::walk(config_yaml$module_files, source)
purrr::walk(config_yaml$page_files, source)

