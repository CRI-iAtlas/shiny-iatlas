config_yaml <- yaml::read_yaml("configuration.yaml")
purrr::walk(config_yaml$libraries, library, character.only = T)
purrr::walk(config_yaml$function_files, source)

# general data loading & prep

DATAMODE <- "SQLite_db"
PANIMMUNE_DB   <- create_db2()
source("database/connect_to_db.R", chdir = TRUE)
PANIMMUNE_DB2 <- .GlobalEnv$connect_to_db()

purrr::walk(config_yaml$module_files, source)
purrr::walk(config_yaml$page_files, source)

