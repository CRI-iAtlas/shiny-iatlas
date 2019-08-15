config_yaml <- yaml::read_yaml("configuration.yaml")
purrr::walk(config_yaml$libraries, library, character.only = T)
purrr::walk(config_yaml$function_files, source)

# general data loading & prep
USE_REMOTE_BQ <- config_yaml$bq_remote 
USE_REMOTE_GS <- config_yaml$gs_remote

panimmune_data <- load_data()

purrr::walk(config_yaml$module_files, source)
purrr::walk(config_yaml$page_files, source)

