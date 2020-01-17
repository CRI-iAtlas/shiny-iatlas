context("utils.R")
library(testthat)
config_yaml <- yaml::read_yaml("../configuration.yaml")
purrr::walk(config_yaml$libraries, library, character.only = T)
source("../functions/utils.R")
# source("../database/connect_to_db.R")
# source("../database/database_functions.R")
# DB_HOST <- "localhost"
# DB_NAME <- "iatlas_dev"
# DB_PORT <- "5432"
# DB_PW <- "docker" 
# DB_USER <- "postgres"
# pool <- connect_to_db()

# connection/tibble/dataframe checkers ----------------------------------------

test_that("assert_data_has_columns", {
    tbl <- dplyr::tibble(
        "col1" = c("value1", "value2"),
        "col2" = c("A", "B"),
        "col3" = c("C", "C")
    )
    
    testthat::expect_that(
        assert_data_has_columns(tbl, c("col1", "col2")),
        testthat::is_identical_to(NULL)
    )
    testthat::expect_that(
        assert_data_has_columns(tbl, c("cola", "col2")),
        testthat::throws_error("data has missing columns: cola")
    )
    testthat::expect_that(
        assert_data_has_columns(tbl, c("cola", "colb")),
        testthat::throws_error("data has missing columns: cola, colb")
    )
    
    # con <- create_conection("genes")
    # testthat::expect_that(
    #     assert_data_has_columns(con, c("id", "entrez")),
    #     testthat::is_identical_to(NULL)
    # )
    # testthat::expect_that(
    #     assert_data_has_columns(con, c("id", "hugo")),
    #     testthat::throws_error("data has missing columns: hugo")
    # )
})

test_that("assert_data_has_rows", {
    tbl1 <- dplyr::tibble("col1" = c("value1", "value2"))
    tbl2 <- dplyr::filter(tbl1, "col1" == "value3")
    
    testthat::expect_that(
        assert_data_has_rows(tbl1),
        testthat::is_identical_to(NULL)
    )
    testthat::expect_that(
        assert_data_has_rows(tbl2),
        testthat::throws_error("data is empty")
    )

    # con1 <- create_conection("genes")
    # con2 <- dplyr::filter(con, id == -1)
    # testthat::expect_that(
    #     assert_data_has_rows(con1),
    #     testthat::is_identical_to(NULL)
    # )
    # testthat::expect_that(
    #     assert_data_has_rows(con2),
    #     testthat::throws_error("data is empty")
    # )

})


