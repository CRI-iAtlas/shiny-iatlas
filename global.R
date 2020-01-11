config_yaml <- yaml::read_yaml("configuration.yaml")
purrr::walk(config_yaml$libraries, library, character.only = T)
purrr::walk(config_yaml$function_files, source)

# general data loading & prep
source("database/connect_to_db.R")
PANIMMUNE_DB2 <- connect_to_db()

purrr::walk(config_yaml$module_files, source)
purrr::walk(config_yaml$page_files, source)

