context("transform.R")
library(testthat)
config_yaml <- yaml::read_yaml("../configuration.yaml")
purrr::walk(config_yaml$libraries, library, character.only = T)
source("../functions/tranform.R")
# source("../database/connect_to_db.R")
# source("../database/database_functions.R")
# DB_HOST <- "localhost"
# DB_NAME <- "iatlas_dev"
# DB_PORT <- "5432"
# DB_PW <- "docker" 
# DB_USER <- "postgres"
# pool <- connect_to_db()

# scale db connection functions -----------------------------------------------

# test_that("log_db_connection", {
#     con <- create_conection("features_to_samples") %>% 
#         dplyr::filter(
#             feature_id == 1,
#             sample_id %in% 1:5
#         ) %>% 
#         dplyr::select(value) 
#     
#     testthat::expect_that(
#         round(dplyr::pull(log_db_connection(con), value)),
#         testthat::is_identical_to(c(2, 2, 2, 2, 2))
#     )
#     
#     testthat::expect_that(
#         round(dplyr::pull(log_db_connection(con, base = 2), value)),
#         testthat::is_identical_to(c(5, 6, 6, 6, 5))
#     )
#     
# })


