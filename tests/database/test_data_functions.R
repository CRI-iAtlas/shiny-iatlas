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

# switch_value
testthat::test_that("switch_value return NA when no value present", {
  reference <- dplyr::tibble(gene = c("1", "2", "3"), scoobs = c(NA, NA, NA))
  object <- dplyr::tibble(gene = c("", "", ""), scoobs = c("4", "5", "6"))

  testthat::expect_that(switch_value(reference[1,], "gene", "scoobs", object), is_identical_to(NA))
})
testthat::test_that("switch_value return the value from the second object.", {
  reference <- dplyr::tibble(gene = c("1", "2", "3"), scoobs = c(NA, NA, NA))
  object <- dplyr::tibble(gene = c("1", "", ""), scoobs = c("4", "5", "6"))

  testthat::expect_that(switch_value(reference[1,], "gene", "scoobs", object), is_identical_to("4"))
})

# link_to_references
testthat::test_that("link_to_references return NA when no value present", {
  testthat::expect_that(link_to_references(NA), is_identical_to(NA))
})
testthat::test_that("link_to_references return NA when URL value is 'NA'", {
  link <- '<a href="NA">NA</a>'
  testthat::expect_that(link_to_references(link), is_identical_to(NA))
})
testthat::test_that("link_to_references return NA when there is no URL", {
  link <- '<a href>NA</a>'
  testthat::expect_that(link_to_references(link), is_identical_to(NA))
})
testthat::test_that("link_to_references return a url in curly braces", {
  url <- 'http://someplace.com?query=yes#pow'
  link <- paste0('<a href="', url, '"></a>', sep = "")
  expected <- paste0('{', url, '}', sep = "")
  testthat::expect_that(link_to_references(link), is_identical_to(expected))
})
