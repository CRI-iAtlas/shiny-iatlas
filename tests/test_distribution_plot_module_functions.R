context("distribution_plot_module_functions.R")
library(testthat)
config_yaml <- yaml::read_yaml("../configuration.yaml")
purrr::walk(config_yaml$libraries, library, character.only = T)
source("../functions/distribution_plot_module_functions.R")


# test_that("get_feature_group_names", {
#     con1 <- create_conection("features") %>% 
#         dplyr::select(feature_id = id, feature_name = name, order, unit)
#     con2 <- dplyr::select(con1, -order)
#     testthat::expect_identical(get_feature_group_names(con1), c("order", "unit"))
#     testthat::expect_identical(get_feature_group_names(con2), "unit")
# })