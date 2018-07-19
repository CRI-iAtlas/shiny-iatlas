library(yaml)
library(tidyverse)

config_yaml <- yaml::read_yaml("../configuration.yaml")
purrr::walk(config_yaml$libraries, library, character.only = T)

source("../functions/utils.R")

test_that("convert_value_between_columns", {
    test_df1 <- data_frame("col1" = c("value1", "value2"),
                           "col2" = c("A", "B"),
                           "col3" = c("C", "C"))
    expect_that(
        convert_value_between_columns(test_df1, "value1", "col1", "col2"), 
        is_identical_to("A"))
    expect_that(
        convert_value_between_columns(test_df1, "value2", "col1", "col2"), 
        is_identical_to("B"))
    expect_that(
        convert_value_between_columns(test_df1, "value1", "col1", "col3"), 
        is_identical_to("C"))
    expect_that(
        convert_value_between_columns(test_df1, "C", "col3", "col1"), 
        is_identical_to(c("value1", "value2")))
    expect_that(
        convert_value_between_columns(test_df1, "value3", "col1", "col2"), 
        is_identical_to(vector(mode = "character", length = 0)))
    expect_that(
        convert_value_between_columns(
            test_df1, "value3", "col1", "col2", return_one_value= T), 
        throws_error("value has no match in new column"))
})


test_that("set_names_to_self", {
    expect_that(
        set_names_to_self(list("A", "B")),
        is_identical_to(list("A" = "A", "B" = "B")))
    expect_that(
        set_names_to_self(c("A", "B")),
        is_identical_to(c("A" = "A", "B" = "B")))
    expect_that(
        set_names_to_self(list()),
        throws_error("imput list/vector empty"))
    expect_that(
        set_names_to_self(c()),
        throws_error("imput list/vector empty"))
})

test_that("get_variable_group", {
    test_df <- data_frame(
        "Variable Class" = c(
            "Overall Proportion", 
            "Overall Proportion", 
            "Overall Proportion",
            "Core Expression Signature", 
            "Core Expression Signature",
            "Core Expression Signature"),
        "FeatureMatrixLabelTSV" = c(
            "leukocyte_fraction",
            "Stromal_Fraction",
            "til_percentage",
            "CHANG_CORE_SERUM_RESPONSE_UP",
            "CSF1_response",
            "LIexpression_score"),
        "Variable Class Order" = c(1, 2, 3, 3, 2, 1))
    labels1 <- c("leukocyte_fraction", "Stromal_Fraction", "til_percentage")
    labels2 <- c("LIexpression_score", "CSF1_response", "CHANG_CORE_SERUM_RESPONSE_UP")
    expect_that(
        get_variable_group("Overall Proportion", test_df),
        is_identical_to(factor(labels1, levels = labels1)))
    expect_that(
        get_variable_group("Core Expression Signature", test_df),
        is_identical_to(factor(labels2, levels = labels2)))
    expect_that(
        get_variable_group("", test_df),
        throws_error("group empty"))
})   

test_that("get_unique_column_values"){
    test_df1 <- data_frame("col" = c("value1", "value2"))
    test_df2 <- data_frame("col" = c("value1", "value1"))
    test_df3 <- data_frame("col" = c("value1", NA))
    test_df4 <- data_frame("col" = c(5, 6))
    test_df5 <- data_frame("col" = c("value2", "value1"))
    expect_that(
        get_unique_column_values("col", test_df1),
        is_identical_to(c("value1", "value2")))
    expect_that(
        get_unique_column_values("col", test_df2),
        is_identical_to(c("value1")))
    expect_that(
        get_unique_column_values("col", test_df3),
        is_identical_to(c("value1")))
    expect_that(
        get_unique_column_values("col", test_df4),
        is_identical_to(c("5", "6")))
    expect_that(
        get_unique_column_values("col", test_df5),
        is_identical_to(c("value1", "value2")))
}