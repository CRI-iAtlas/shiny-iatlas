# Ensure magrittr is installed.
# if (!'magrittr' %in% installed.packages()) {
#   install.packages("magrittr")
# }

# Ensure RPostgres is installed.
# if (!'RPostgres' %in% installed.packages()) {
#   install.packages("RPostgres")
# }

# Load magrittr so %>% is available.
library("magrittr")

# Loading RPostgres loads DBI automatically.
library("RPostgres")

# Loading testthat (used for unit tests).
library("testthat")

# Make the custom data functions available.
source("../../database/data_functions.R", chdir = TRUE)

# filter_na
testthat::test_that("filter_na returns the value when the passed value is NOT NA.", {
  testthat::expect_that(filter_na(c(14)), is_identical_to(14))
  testthat::expect_that(filter_na(14), is_identical_to(14))
})
testthat::test_that("filter_na returns the value when the passed value is combined with an NA.", {
  testthat::expect_that(filter_na(c(14, NA)), is_identical_to(14))
})
# testthat::test_that("filter_na returns NA when there is no passed value or the passed value is NA.", {
#   cat("value:", filter_na(NA), fill = TRUE, sep = " ")
#   testthat::expect_that(filter_na(NA), is_identical_to(NA %>% as.character))
# })

# is_df_empty
testthat::test_that("is_df_empty returns FALSE when a valid non-empty dataframe or tibble is passed.", {
  testthat::expect_that(is_df_empty(cars), is_identical_to(FALSE))
  testthat::expect_that(is_df_empty(cars %>% dplyr::as_tibble()), is_identical_to(FALSE))
})
testthat::test_that("is_df_empty returns TRUE when an empty dataframe, empty tibble, NA, NULL, or no data frame is passed.", {
  testthat::expect_that(is_df_empty(data.frame()), is_identical_to(TRUE))
  testthat::expect_that(is_df_empty(NA), is_identical_to(TRUE))
  testthat::expect_that(is_df_empty(NULL), is_identical_to(TRUE))
  testthat::expect_that(is_df_empty(), is_identical_to(TRUE))
})

# link_to_references
testthat::test_that("link_to_references returns NA when no value present.", {
  testthat::expect_that(link_to_references(NA), is_identical_to(NA))
})
testthat::test_that("link_to_references returns NA when URL value is 'NA'.", {
  link <- '<a href="NA">NA</a>'
  testthat::expect_that(link_to_references(link), is_identical_to(NA))
})
testthat::test_that("link_to_references returns NA when there is no URL.", {
  link <- '<a href>NA</a>'
  testthat::expect_that(link_to_references(link), is_identical_to(NA))
})
testthat::test_that("link_to_references returns a url in curly braces.", {
  url <- 'http://someplace.com?query=yes#pow'
  link <- paste0('<a href="', url, '"></a>', sep = "")
  expected <- paste0('{', url, '}', sep = "")
  testthat::expect_that(link_to_references(link), is_identical_to(expected))
})

# switch_value
testthat::test_that("switch_value returns NA when no value present.", {
  reference <- dplyr::tibble(gene = c("1", "2", "3"), scoobs = c(NA, NA, NA))
  object <- dplyr::tibble(gene = c("", "", ""), scoobs = c("4", "5", "6"))

  testthat::expect_that(switch_value(reference[1,], "gene", "scoobs", object), is_identical_to(NA))
})
testthat::test_that("switch_value returns the value from the second object.", {
  reference <- dplyr::tibble(gene = c("1", "2", "3"), scoobs = c(NA, NA, NA))
  object <- dplyr::tibble(gene = c("1", "", ""), scoobs = c("4", "5", "6"))

  testthat::expect_that(switch_value(reference[1,], "gene", "scoobs", object), is_identical_to("4"))
})
