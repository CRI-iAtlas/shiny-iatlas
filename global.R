library(shiny)
library(shinydashboard)
library(plotly)



config_yaml <- yaml::read_yaml("configuration.yaml")
purrr::walk(config_yaml$function_files, source)
purrr::walk(config_yaml$plot_files, source)

# general data loading & prep
source("database/connect_to_db.R")
pool <- connect_to_db()



